{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , LambdaCase
           , MultiParamTypeClasses
           , OverloadedStrings
           #-}
module Language.Alonzo.Transform.ANF where

import Data.Map (Map)
import Data.Maybe
import Data.Typeable (Typeable)
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Alonzo.Syntax.Location
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Language.Alonzo.Syntax.Builtin as BI
import qualified Language.Alonzo.Transform.Abstract as B

--------------------------------------------------------------------------------------------------
-- Term Definition

data Closure = Closure (Bind (Rec [(Var, Embed Term)]) [Term])
  deriving(Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Term Definition

type Var = Name Val

data Term
  = TVal Val
  | TPrim BI.PrimInstr Val Val
  | TApp Val [Val]
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)
  | TLoc Loc Term
  deriving(Show, Generic, Typeable)

data Val
  = VVar Var
  | VLam (Bind [Var] Term)
  | VVal BI.Val
  | VLoc Loc Val
  | VWild
  deriving(Show, Generic, Typeable)


instance Alpha Term
instance Alpha Val

instance Subst Val Loc
instance Subst Val Region
instance Subst Val Position
instance Subst Val BI.Val
instance Subst Val BI.PrimInstr

instance Subst Val Term
instance Subst Val Val where
  isvar (VVar x) = Just (SubstName x)
  isvar _ = Nothing


name :: B.Var -> Var
name = string2Name . name2String

lam :: [Var] -> Term -> Term
lam vs body =
  TVal . VLam $ bind vs body

let_ :: [(Var, Term)] -> Term -> Term
let_ [] b = b
let_ bs b =
  let bs' = rec [ (v, embed t) | (v, t) <- bs ]
  in TLet $ bind bs' b


free :: Term -> [Var]
free t =
  toListOf (fv :: Fold Term (Name Val)) t


-- foo = \x y. f (g x) (h y)
--      = \x y. 
--        let x1 = g x
--            x2 = h y
--        in  f x1 x2

normalize :: B.Closure -> Closure
normalize = runFreshM . anfClosure

anfClosure :: B.Closure -> FreshM Closure
anfClosure cl = undefined

anf :: B.Term -> FreshM Term
anf = \case
  B.TVar v -> return . TVal . VVar $ name v
  B.TVal v -> return . TVal $ VVal v
  
  B.TPrim i a b -> do
    ([a', b'], brs) <- anfVals [a, b]
    return $ let_ brs (TPrim i a' b')

  B.TApp f xs -> do
    (f':xs', brs) <- anfVals (f:xs)
    return $ let_ brs (TApp f' xs')

  B.TLam bs -> do
    (vs, t) <- unbind bs
    t' <- anf t
    return $ lam (name <$> vs) t' 

  B.TLet bs -> do
    (brs, body) <- unbind bs
    brs' <- mapM (\(v, t) -> anf (unembed t) >>= \t' -> return (name v, t')) (unrec brs)
    body' <- anf body
    return $ let_ brs' body'

  B.TLoc l t -> anf t >>= (return . TLoc l)
  B.TWild -> return $ TVal VWild


anfVals :: [B.Term] -> FreshM ([Val], [(Var, Term)])
anfVals ts = do
  rs <- mapM anfVal ts
  let vs = fst <$> rs
      ds = catMaybes $ snd <$> rs
  return (vs, ds)

anfVal :: B.Term -> FreshM (Val, Maybe (Var, Term))
anfVal tm = case tm of
  B.TVar v -> return (VVar (name v), Nothing)
  B.TVal v -> return (VVal v, Nothing)
  
  B.TLoc l t -> do
    (v, mb) <- anfVal t
    case mb of
      Nothing       -> return (VLoc l v, Nothing)
      Just (var, f) -> return (VLoc l v, Just (var, TLoc l f))

  B.TWild   -> return (VWild, Nothing)

  _ -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))


instance Pretty Term where
  pretty = runFreshM . prettyFreshTerm

instance Pretty Val where
  pretty = runFreshM . prettyFreshVal


prettyFreshTerm :: Term -> FreshM (Doc ann)
prettyFreshTerm = \case
  TVal v -> prettyFreshVal v

  TPrim i t1 t2 -> do
    t1' <- prettyFreshVal t1
    t2' <- prettyFreshVal t2
    return $ pretty i <+> t1' <+> t2'
  
  TApp f as -> do
    f' <- prettyFreshVal f 
    as' <- mapM prettyFreshVal as
    return $ f' <+> hsep as'

  TLet bnd -> do
    (brs, body) <- unbind bnd
    brs' <- mapM (\(v, t) -> prettyFreshTerm (unembed t) >>= \t' -> return (pretty (show v) <+> "=" <+> t')) (unrec brs)
    body' <- prettyFreshTerm body
    return $ "let" <+> hsep (punctuate comma brs') <+> "in" <+> body'
  
  TLoc _ t -> prettyFreshTerm t



prettyFreshVal :: Val -> FreshM (Doc ann)
prettyFreshVal = \case  
  VVar v        -> return . pretty . show $ v
  VLam bnd -> do
    (xs, body) <- unbind bnd
    let xs' = (pretty . show) <$> xs 
    body' <- prettyFreshTerm body
    return $ "\\" <> hsep xs' <> "." <+> body'

  VVal v   -> return $ pretty v
  VLoc _ v -> prettyFreshVal v
  VWild    -> return "_"
