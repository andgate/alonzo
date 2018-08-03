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
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Alonzo.Repl.Error
import Language.Alonzo.Lex (lex)
import Language.Alonzo.Lex.Token (Token)
import Language.Alonzo.Parse
import Language.Alonzo.Rename (rename)
import Language.Alonzo.Syntax.Module
import Language.Alonzo.Transform.Eval (eval)
import Language.Alonzo.Value
import System.IO (hFlush, stdout)


import qualified Language.Alonzo.Closure as CL
import qualified Language.Alonzo.Syntax.Source as S
import qualified Language.Alonzo.Syntax.Bound as B

import qualified Data.Text.IO as T


data ReplState
  = ReplState
    { _replQuit :: Bool
    , _replMod :: Text
    , _replPrj :: Project
    , _replHistory :: [String]
    }

makeLenses ''ReplState

initialReplState :: ReplState
initialReplState =
  ReplState
    { _replQuit = False
    , _replMod = "repl"
    , _replPrj = newProject "repl"
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
repl = untilM_ (read' >>= traverse execute >> liftIO (putStr "\n") ) (use replQuit)

read' :: Repl (Maybe S.Decl)
read' = do
  printDecoration
  ln <- liftIO T.getLine
  r <- bitraverse (\err -> liftIO ( putDoc (pretty err) >> T.putStr "\n"))
                  return
                  (parseDecl "repl" ln)
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
  S.TermDecl t -> frontend t >>= traverse eval' >> return ()
  _ -> error "unsupported input"


frontend :: S.Term -> Repl (Maybe B.Term)
frontend = rename'

eval' :: B.Term -> Repl B.Term
eval' t = do
  let r = eval CL.empty t
  liftIO $ do
    putDoc . pretty $ r
    putStr "\n"
  return r

rename' :: S.Term -> Repl (Maybe B.Term)
rename' t = case rename [] t of
  Left err -> do 
    liftIO . putDoc . pretty $ err
    return Nothing

  Right t' -> do
    -- liftIO . putStr . show $ t'
    return $ Just t'

loadPrelude :: Repl ()
loadPrelude = do
  preludeSrc <- liftIO $ T.readFile "prelude/Prelude.al"
  -- Parse, load defs
  return ()