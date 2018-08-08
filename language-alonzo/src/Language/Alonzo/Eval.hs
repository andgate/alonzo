{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
 #-}
module Language.Alonzo.Eval where


import Control.Lens hiding (toListOf)
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Language.Alonzo.Transform.ANorm
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import qualified Data.Map.Strict as Map

type Closure = Map Text Exp

reduce :: Closure -> Exp -> Val
reduce cl = runFreshM . reduceExp cl


reduceExp :: Closure -> Exp -> FreshM Val
reduceExp cl = \case
  EVal v -> return v
  
  EPrim i a b -> do
    case (a, b) of
      (VVal v1, VVal v2) ->
           return . VVal $ evalInstr (i, v1, v2)
      (VLoc _ a, b) -> reduceExp cl $ EPrim i a b
      (a, VLoc _ b) -> reduceExp cl $ EPrim i a b
      _ -> error "Primitive instruction encountered non-primitive values"

  EApp f []     -> return f
  EApp f (a:as) ->
    case f of
      VLam bnd -> do
        (xs, body)   <- unbind bnd
        case xs of
          []    -> reduceExp cl body
          x:[]  -> do
            f' <- reduceExp cl (subst x a body)
            case as of
              [] -> return f'
              _  -> reduceExp cl $ EApp f' as
              
          x:xs' ->
            let body' = subst x a body
                f' = VLam $ bind xs' body'
            in reduceExp cl $ EApp f' as

      VVar v ->
        case Map.lookup (pack $ name2String v) cl of
          Nothing -> error $ "undeclared variable encountered: " ++ (name2String v)
          Just t -> do
            t' <- reduceExp cl t
            reduceExp cl $ EApp t' (a:as)

      VLoc _ f' -> reduceExp cl $ EApp f' (a:as)

      _ -> error $ "Application head must be a lambda term: " ++ (show $ pretty $ EApp f (a:as))


  ELet bnd -> do
    (r, body) <- unbind bnd
    let bs = unrec r
    bs' <- mapM (\(v, rhs) -> reduceExp cl (unembed rhs) >>= \rhs' -> return (v, rhs')) (unrec r)

        -- Substitute all terms into the body
    let body' = foldr (\(v, rhs) body' -> subst v rhs body') body bs'
        fvs = toListOf fv body'
    if any (\(v,_) -> v `elem` fvs) bs
      then reduceExp cl . ELet $ bind r body'
      else reduceExp cl body'

  ELoc _ e   -> reduceExp cl e


-- ELet [("true", EVal (ELam ["t", "f"] (EVal $ VVar "t"))] (EApp (VVar "true") [VVal 1, VVal 0])