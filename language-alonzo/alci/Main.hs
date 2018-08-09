{-# Language TemplateHaskell
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , LambdaCase
  #-}
module Main where

import Prelude hiding (lex)

import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Bitraversable
import Data.Either
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Alonzo.Analysis
import Language.Alonzo.Analysis.NameCheck (namecheck, NameError(..))
import Language.Alonzo.Analysis.Error
import Language.Alonzo.Repl.Error
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse
import Language.Alonzo.Parse.Error
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Transform
import Language.Alonzo.Transform.Reduce (reduce, Closure)
import System.IO (hFlush, stdout)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language.Alonzo.Syntax.Source      as S
import qualified Language.Alonzo.Transform.NameBind as B
import qualified Language.Alonzo.Transform.ANorm    as A

import qualified Data.Text.IO as T


data ReplState
  = ReplState
    { _replQuit :: Bool
    , _replMod :: Text
    , _replClosure :: Map Text A.Term
    , _replHistory :: [String]
    }

makeLenses ''ReplState

initialReplState :: ReplState
initialReplState =
  ReplState
    { _replQuit = False
    , _replMod = "repl"
    , _replClosure = Map.empty
    , _replHistory = []
    }



newtype Repl a = Repl { unRepl :: StateT ReplState (ExceptT ReplError IO) a }
  deriving (Functor, Applicative, Monad, MonadState ReplState, MonadError ReplError, MonadIO)

runRepl :: Repl a -> IO a
runRepl m = do
  r <- runExceptT $ evalStateT (unRepl m) initialReplState
  case r of
    Left err -> do -- Don't end up here!!
      putDoc $ pretty err
      error "Severe error encountered. Repl aborted."

    Right a  -> return a

main :: IO ()
main = runRepl (loadPrelude >> repl)

repl :: Repl ()
repl = untilM_ handleProcess (use replQuit)

printDecoration :: Repl ()
printDecoration = do
  dstr <- _replMod <$> get
  liftIO $ do
    T.putStr dstr
    T.putStr " > "
    hFlush stdout

handleProcess :: Repl ()
handleProcess = catchError process handle
  where handle = \case
          ReplParseErr     -> return ()
          ReplAnalysisErr  -> return ()
          ReplSeriousErr e -> throwError $ ReplSeriousErr e

process :: Repl ()
process = do
  printDecoration
  srctxt <- liftIO T.getLine
  parse' srctxt >>= \t -> case t of 
    S.TermDecl t  -> evalTerm t >>= printPretty
    S.FunDecl n t -> saveDef n t
  liftIO (putStr "\n")

parse' :: Text -> Repl S.Decl
parse' src = case parseDecl "repl" src of
  Right t -> return t
  Left e  -> do
    printPretty e
    throwError ReplParseErr


saveDef :: Text -> S.Term -> Repl ()
saveDef n t = do
  ns <- uses replClosure Map.keys
  t' <- transform' =<< namecheck' (n:ns) t
  replClosure %= Map.insert n t'


evalTerm :: S.Term -> Repl A.Val
evalTerm t = do
  cl <- (Map.keys . _replClosure) <$> get
  namecheck' cl t >>= transform' >>= reduce'
  


printPretty :: Pretty p => p -> Repl ()
printPretty p =
  liftIO $ putDoc (pretty p) >> putStr "\n"


transform' :: S.Term -> Repl A.Term
transform' t = do 
  liftIO $ putStr "Source:"
  printPretty t
  return $ transform t

reduce' :: A.Term -> Repl A.Val
reduce' t = do
  liftIO $ putStr "Anormalized:"
  printPretty t
  cl <- _replClosure <$> get
  return $ reduce cl t
  

namecheck' :: [Text] -> S.Term -> Repl S.Term
namecheck' ns t = case namecheck (Set.fromList ns) (S.locOf t) t of
  [] -> return t
  es -> do
    let errMsg = vcat (pretty <$> es) 
    liftIO $ putDoc errMsg >> T.putStr "\n"
    throwError ReplAnalysisErr


loadPrelude :: Repl ()
loadPrelude = loadFile "prelude/Prelude.al"

loadFile :: FilePath -> Repl ()
loadFile fp = (loadFile' fp) `catchError` handle
  where
    handle (ReplSeriousErr msg) = error (unpack msg)
    handle e = liftIO . putDoc . pretty $ e

loadFile' :: FilePath -> Repl ()
loadFile' fp = do
  src <- liftIO $ T.readFile fp
  case parseFile fp src of
    Left e   -> fileParseFail fp e
    Right ds -> do
      buildFile fp ds
      liftIO $ putStr ("Loaded: " ++ fp ++ "\n")

buildFile :: FilePath -> [S.Decl] -> Repl ()
buildFile fp ds = do
  let ps@(cl, ts) = S.programs ds
  -- checkPrograms fp ps
  installPrograms cl
  runPrograms ts


checkPrograms :: FilePath -> S.Programs -> Repl ()
checkPrograms fp ps@(cl, ts) = do
  detectClosureConflict fp cl
  replCl <- use replClosure
  let ns = Map.keys replCl ++ Map.keys cl
  -- TODO: Actually check the prelude names
  return ()


installPrograms :: S.Closure -> Repl ()
installPrograms cl = do
  let ts = Map.elems cl
  ts' <- mapM transform' ts

  let ns = Map.keys cl
      cl' = Map.fromList (zip ns ts')
  replClosure %= Map.union cl'

  mapM_ (installed . unpack) ns
  
  


runPrograms :: [S.Term] -> Repl ()
runPrograms ts =
  -- TODO: Run programs specified in prelude
  return ()



detectClosureConflict :: FilePath -> S.Closure -> Repl ()
detectClosureConflict fp cl = do
  nsRepl <- Map.keys <$> use replClosure
  case Map.keys cl `List.intersect` nsRepl of
    [] -> return ()
    ns -> closureConflict fp ns -- print error, throw exception



nameErrors :: FilePath -> [NameError] -> Repl ()
nameErrors fp es = do
  liftIO $ do
    putDoc $ vsep (pretty <$> es)
    putStr "\n"
  throwError $ ReplFileLoadErr fp


closureConflict :: FilePath -> [Text] -> Repl ()
closureConflict fp ns = do
  liftIO $ do
    T.putStr "Conflicting names detected:\n"
    putDoc $ hsep (pretty <$> ns)
    putStr "\n"
  throwError $ ReplFileLoadErr fp


fileParseFail :: FilePath -> ParseError -> Repl ()
fileParseFail fp e =
  liftIO $ do
    putDoc (pretty e)
    putStr fp
    T.putStr "\nFailed to load "
    putStr (fp ++ "\n")


installed :: String -> Repl ()
installed n = liftIO $ putStr ("Installed: " ++ n ++ "\n")