{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Transform.CConv where

import Data.Set (Set)
import Language.Alonzo.Transform.ANF
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Data.Set as Set


cconv :: Term -> Term
cconv e = runFreshM (cconvTerm Set.empty e)


cconvTerm :: Set Var -> Term -> FreshM Term
cconvTerm cl = \case
  TVal v -> case v of
    VLam bnd   -> do
      let fv = Set.fromList . free $ TVal v
      let cap = Set.toList (fv `Set.intersection` cl)
      
      (as, body) <- unbind bnd
      let cl' = cl `Set.union` Set.fromList as
      let as' = cap ++ as
      body' <- cconvTerm cl' body

      let f = VLam $ bind as' body'
      return $ TApp f (VVar <$> cap)
      

    VLoc l v -> TLoc l <$> cconvTerm cl (TVal v)
    _        -> return $ TVal v
  
  TPrim i a b -> return $ TPrim i a b -- In ANF, so TPrim will have no lambdas in argument
  TApp f as   -> return $ TApp f as   -- In ANF, so no lambdas here

  TLet bnd  -> do
    (r, body) <- unbind bnd

    let bs = unrec r
        vs = Set.fromList (fst <$> bs)
        cl' = cl `Set.union` vs

    bs' <- mapM (\(v, rhs) -> cconvTerm cl' (unembed rhs) >>= \rhs' -> return (v, embed rhs')) bs
    -- Add let vars to closure?
    body' <- cconvTerm cl' body
    return . TLet $ bind (rec bs') body'

  TLoc l t   -> TLoc l <$> cconvTerm cl t