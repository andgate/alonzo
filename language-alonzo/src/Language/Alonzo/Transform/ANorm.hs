{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Transform.ANorm where

import Data.Typeable (Typeable)
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Language.Alonzo.Syntax.Bound as B


type Var = Name Exp

data Exp
  = EVal Val
  | EApp Val [Val]
  | EPrim PrimInstr Val Val
  | ELet (Bind (Rec [(Var, Embed Exp)]) Exp)
  | ELoc Loc Term
  deriving(Show, Generic, Typeable)

data Val
  = VLam (Bind [Var] Exp)
  | VVar Var
  | VVal PrimVal
  | VLoc Loc Val
  | VWild
  deriving(Show, Generic, Typeable)

anf :: B.Term -> FreshM Exp
anf = \case
  B.TVar v -> return . EVal . VVar $ v
  B.TVal v -> return . EVal . VVal $ v
  
  B.TPrim v ->

  B.TApp f xs -> do
    (f', mfb) <- anfVal f
    vs' <- mapM anfVal xs
    
    in ELet (bind vs (EApp f' xs')

  B.TLam bs -> do
    (vs, t) <- unbind bs
    t' <- anf t
    return . EVal . VLam $ bind vs t' 

  B.TLet bs -> undefined

  B.TLoc l t -> anf t >>= (return . ELoc l)
  B.TWild -> return $ EVal VWild


anfVal :: B.Term -> FreshM (Val, Maybe (Var, Exp))
anfVal tm = case tm of
  B.TVar v -> return (VVar v, Nothing)
  B.TVal v -> return (VVal v, Nothing)
  
  B.TPrim _ _ _ -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))

  B.TApp _ _ -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))

  B.TLam _   -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))

  B.TLet _   -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))

  B.TLoc l t -> do
    mv <- anfVal t
    case mv of
      Nothing -> return Nothing
      Just mv -> return $ VLoc l v

  B.TWild   -> return $ EVal VWild


foo = \x y. f (g x) (h y)
    = \x y. 
      let x1 = g x
          x2 = h y
      in f x1 x2