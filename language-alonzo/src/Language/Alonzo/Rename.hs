{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , GADTs
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , ExistentialQuantification
           , TemplateHaskell
  #-}
module Language.Alonzo.Rename where

import Control.Lens
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
  { _rnLoc     :: Loc
  , _rnGlobals :: Set String
  }

makeLenses  ''REnv

newREnv :: [String] -> Loc -> REnv
newREnv vs l 
  = REnv { _rnLoc = l
         , _rnGlobals = Set.fromList vs
         }

newtype Renamer a = Renamer { unRenamer :: ReaderT REnv (Except RenameError) a }
  deriving (Functor, Applicative, Monad, MonadReader REnv, MonadError RenameError)


rename :: [String] -> S.Term -> Either RenameError Term
rename vs t = runRenamer (newREnv vs (locOf t)) $ renameTerm t


runRenamer :: REnv -> Renamer a -> Either RenameError a
runRenamer env rn = runExcept $ runReaderT (unRenamer rn) env

renameTerm :: S.Term -> Renamer Term
renameTerm = \case

  S.TVar n    -> return . tvar $ unpack n

  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam vs body -> do
    let vs' = unpack <$> NE.toList vs
    chkDups vs'
    body' <- renameTerm body
    return $ tlam vs' body'

  S.TLet fns body ->
    tlet <$> renameFuns (NE.toList fns) <*> renameTerm body

  S.TLoc l t   -> TLoc l <$> local (rnLoc .~ l) (renameTerm t)
  S.TParens t  -> renameTerm t
  S.TWild      -> return TWild


getDups :: [String] -> [String]
getDups vs = go vs Set.empty []
  where
    go :: [String] -> Set String -> [String] -> [String]
    go [] _ ds = reverse ds
    go (v:vs) xs ds 
      | v `Set.member` xs = go vs xs (v:ds) 
      | otherwise         = go vs (Set.insert v xs) ds


chkDups :: [String] -> Renamer ()
chkDups vs =
  case getDups vs of
      [] -> return ()
      ds -> do 
        l <- view rnLoc
        throwError $ NameCollision l ds

renameFuns :: [(Text, S.Term)] -> Renamer [(String, Term)]
renameFuns fns =
  traverse renameFun fns 

renameFun :: (Text, S.Term) -> Renamer (String, Term)
renameFun (n, t) = do
  t' <- renameTerm t
  return (unpack n, t')