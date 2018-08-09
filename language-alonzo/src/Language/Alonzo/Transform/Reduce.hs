{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
 #-}
module Language.Alonzo.Transform.Reduce where


import Control.Lens hiding (toListOf)
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Language.Alonzo.Transform.ANorm
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import qualified Data.Map.Strict as Map

type Closure = Map Text Term

reduce :: Closure -> Term -> Val
reduce cl = runFreshM . reduceTerm cl


reduceTerm :: Closure -> Term -> FreshM Val
reduceTerm cl = \case
  TVal v -> return v
  
  TPrim i a b -> do
    case (a, b) of
      (VVal v1, VVal v2) ->
           return . VVal $ evalInstr (i, v1, v2)
      (VLoc _ a, b) -> reduceTerm cl $ TPrim i a b
      (a, VLoc _ b) -> reduceTerm cl $ TPrim i a b
      _ -> error "Primitive instruction encountered non-primitive values"

  TApp f []     -> return f
  TApp f (a:as) ->
    case f of
      VLam bnd -> do
        (xs, body)   <- unbind bnd
        case xs of
          []    -> reduceTerm cl body
          x:[]  -> do
            f' <- reduceTerm cl (subst x a body)
            case as of
              [] -> return f'
              _  -> reduceTerm cl $ TApp f' as
              
          x:xs' ->
            let body' = subst x a body
                f' = VLam $ bind xs' body'
            in reduceTerm cl $ TApp f' as

      VVar v ->
        case Map.lookup (pack $ name2String v) cl of
          Nothing -> error $ "undeclared variable encountered: " ++ (name2String v)
          Just t -> do
            t' <- reduceTerm cl t
            reduceTerm cl $ TApp t' (a:as)

      VLoc _ f' -> reduceTerm cl $ TApp f' (a:as)

      _ -> error $ "Application head must be a lambda term: " ++ (show $ pretty $ TApp f (a:as))


  TLet bnd -> do
    (r, body) <- unbind bnd
    let bs = unrec r
    bs' <- mapM (\(v, rhs) -> reduceTerm cl (unembed rhs) >>= \rhs' -> return (v, rhs')) (unrec r)

        -- Substitute all terms into the body
    let body' = foldr (\(v, rhs) body' -> subst v rhs body') body bs'
        fvs = toListOf fv body'
    if any (\(v,_) -> v `elem` fvs) bs
      then reduceTerm cl . TLet $ bind r body'
      else reduceTerm cl body'

  TLoc _ t   -> reduceTerm cl t