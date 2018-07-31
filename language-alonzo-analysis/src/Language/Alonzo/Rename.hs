{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , GADTs
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , ExistentialQuantification
  #-}
module Language.Alonzo.Rename where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Set (Set)
import Data.Text (Text, unpack)
import Language.Alonzo.Rename.Error
import Language.Alonzo.Syntax.Bound
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless


import qualified Data.Set as Set
import qualified Language.Alonzo.Syntax.Source as S


import qualified Data.List.NonEmpty             as NE


data REnv = REnv
  { _rnLoc   :: Maybe Loc
  , _rnNames :: Set String
  }

newREnv :: [String] -> REnv
newREnv gs = REnv { _rnLoc = Nothing, _rnNames = Set.fromList gs }

newtype Renamer a = Renamer { unRenamer :: ReaderT REnv (Except RenameError) a }
  deriving (Functor, Applicative, Monad, MonadReader REnv, MonadError RenameError)


rename :: [String] -> S.Term -> Either RenameError Term
rename gs t = runRenamer (newREnv gs) $ renameTerm t


runRenamer :: REnv -> Renamer a -> Either RenameError a
runRenamer env rn = runExcept $ runReaderT (unRenamer rn) env



renameTerm :: S.Term -> Renamer Term
renameTerm = \case

  S.TVar n    -> return . tvar $ unpack n

  S.TCon n    -> return . TCon $ unpack n

  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam ps body -> do
    vs <- traverse renamePat (NE.toList ps)
    body' <- renameTerm body
    return $ tlam vs body'

  S.TLet bs body ->
    tlet <$> traverse renamePatBind (NE.toList bs) <*> renameTerm body

  S.TLoc l t   -> TLoc l <$> renameTerm t
  S.TParens t  -> renameTerm t
  S.TWild      -> return TWild


renamePat :: S.Pat -> Renamer String
renamePat p =
  return . unpack $ S.pvarFind' p

renamePatBind :: S.PatBind -> Renamer (String, Term)
renamePatBind (S.PatBind p t) = do
  v  <- renamePat p
  t' <- renameTerm t
  return (v, t')