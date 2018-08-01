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
  , _rnConstrs :: Set String
  }

makeLenses  ''REnv

newREnv :: [String] -> [String] -> Loc -> REnv
newREnv vs cs l 
  = REnv { _rnLoc = l
         , _rnGlobals = Set.fromList vs
         , _rnConstrs = Set.fromList cs 
         }

newtype Renamer a = Renamer { unRenamer :: ReaderT REnv (Except RenameError) a }
  deriving (Functor, Applicative, Monad, MonadReader REnv, MonadError RenameError)


rename :: [String] -> [String] -> S.Term -> Either RenameError Term
rename vs cs t = runRenamer (newREnv vs cs (locOf t)) $ renameTerm t


runRenamer :: REnv -> Renamer a -> Either RenameError a
runRenamer env rn = runExcept $ runReaderT (unRenamer rn) env



renameTerm :: S.Term -> Renamer Term
renameTerm = \case

  S.TVar n    -> return . tvar $ unpack n

  S.TCon n    -> do
    let n' = unpack n
    chkCon n'
    return $ TCon n'

  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam ps body -> do
    let vs = pat2String <$> NE.toList ps
    chkDups vs
    body' <- renameTerm body
    return $ tlam vs body'

  S.TLet bs body ->
    tlet <$> traverse patBind2Var (NE.toList bs) <*> renameTerm body

  S.TLoc l t   -> TLoc l <$> local (rnLoc .~ l) (renameTerm t)
  S.TParens t  -> renameTerm t
  S.TWild      -> return TWild


pat2Var :: S.Pat -> Var
pat2Var
  = string2Name . pat2String

pat2String :: S.Pat -> String
pat2String
  = unpack . S.pvarFind'

patBind2Var :: S.PatBind -> Renamer (String, Term)
patBind2Var (S.PatBind p t) = do
  t' <- renameTerm t
  return (pat2String p, t')


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


chkCon :: String -> Renamer ()
chkCon c = do
  cs <- view rnConstrs
  if c `elem` cs
    then return ()
    else do
      l <- view rnLoc
      throwError $ UndeclaredConstr l c