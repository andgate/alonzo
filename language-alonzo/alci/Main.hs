{-# Language TemplateHaskell
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , LambdaCase
  #-}
module Main where

import Prelude hiding (lex)

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (untilM_)
import Data.Bifunctor
import Data.Bitraversable
import Data.Either
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Alonzo.Repl.Error
import Language.Alonzo.Lex (lex)
import Language.Alonzo.Lex.LFCut (lfCut)
import Language.Alonzo.Lex.Organize (organize)
import Language.Alonzo.Lex.Token (Token)
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse
import Language.Alonzo.Parse.Error
import Language.Alonzo.Transform.NameBind (namebind)
import Language.Alonzo.Transform.ANorm (anf)
import Language.Alonzo.Eval (eval, Closure)
import System.IO (hFlush, stdout)


import qualified Data.Map.Strict as Map
import qualified Language.Alonzo.Syntax.Source      as S
import qualified Language.Alonzo.Transform.NameBind as B

import qualified Data.Text.IO as T


data ReplState
  = ReplState
    { _replQuit :: Bool
    , _replMod :: Text
    , _replClosure :: Map Text B.Term
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
repl = untilM_ (
  printDecoration >> liftIO T.getLine >>= parse' >>= traverse execute >> liftIO (putStr "\n") ) (use replQuit)

parse' :: Text -> Repl (Maybe S.Decl)
parse' src = do
  r <- bitraverse (\err -> liftIO ( putDoc (pretty err) >> T.putStr "\n"))
                  return
                  (parseDecl "repl" src)
  return $ eitherToMaybe r


printDecoration :: Repl ()
printDecoration = do
  dstr <- _replMod <$> get
  liftIO $ do
    T.putStr dstr
    T.putStr " > "
    hFlush stdout

execute :: S.Decl -> Repl ()
execute = \case
  S.TermDecl t -> do
    cl <- _replClosure <$> get
    eval' cl (namebind t)
    return ()

  S.FunDecl n t -> saveDef n (namebind t)


eval' :: Closure -> B.Term -> Repl B.Term
eval' cl t = do
  let r = eval cl t
  liftIO $ do
    putDoc . pretty $ r
    putStr "\n"
  return r

loadPrelude :: Repl ()
loadPrelude = do
  let fp = "prelude/Prelude.al"
  preludeSrc <- liftIO $ T.readFile fp

  ds <- bitraverse (\err -> liftIO ( putDoc (pretty err) >> T.putStr "\n"))
                         return
                         (parseFile fp preludeSrc)
  traverse (mapM_ execute) ds
  return ()


saveDef :: Text -> B.Term -> Repl ()
saveDef n t = do
  s <- get
  let gs = Map.insert n t (_replClosure s)
  put (s {_replClosure = gs})