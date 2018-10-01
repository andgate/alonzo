{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , FlexibleContexts
 #-}
module Language.Alonzo.Transform.Reduce where


import Control.Lens hiding (toListOf)
import Control.Monad.Except
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Language.Alonzo.Transform.ANF
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Fresh
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import qualified Data.Map.Strict as Map




data ReduceErr
  = PrimOpFailure
  | UndeclaredVar String
  | AppHeadLambda Term


instance Pretty ReduceErr where
  pretty = \case
    PrimOpFailure   -> "Primitive instruction encountered non-primitive values"
    UndeclaredVar n -> "Undeclared variable encountered:" <+> pretty n
    AppHeadLambda t -> "Application head must be lambda term:" <+> pretty t



newtype ReduceM a = ReduceM { unReduceM :: ExceptT ReduceErr FreshM a }
  deriving (Functor, Applicative, Monad, MonadError ReduceErr, Fresh)


reduce :: Closure -> [Either ReduceErr Val]
reduce = runFreshM . reduceClosure


reduceClosure :: Closure -> FreshM [Either ReduceErr Val]
reduceClosure (Closure bnd) = do
  (ds, ps) <- unbind bnd
  -- Prep closure for reduction 
  let ns = (name2String . fst) <$> unrec ds
      ts = (unembed . snd) <$> unrec ds
      cl' = Map.fromList (zip ns ts)

  -- Reduce given programs
  mapM (runExceptT . reduceTerm cl') ps


reduceTerm :: (MonadError ReduceErr m, Fresh m) => Map String Term -> Term -> m Val
reduceTerm cl = \case
  TVal v -> return v
  
  TPrim i a b -> do
    case (a, b) of
      (VVal v1, VVal v2) ->
           return . VVal $ evalInstr (i, v1, v2)
      (VLoc _ a, b) -> reduceTerm cl $ TPrim i a b
      (a, VLoc _ b) -> reduceTerm cl $ TPrim i a b
      _ -> throwError PrimOpFailure

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
        case Map.lookup (name2String v) cl of
          Nothing -> throwError $ UndeclaredVar (name2String v)
          Just t -> do
            t' <- reduceTerm cl t
            reduceTerm cl $ TApp t' (a:as)

      VLoc _ f' -> reduceTerm cl $ TApp f' (a:as)

      _ -> throwError $ AppHeadLambda (TApp f (a:as))


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