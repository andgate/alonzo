{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , LambdaCase
           , MultiParamTypeClasses
           , OverloadedStrings
           #-}
module Language.Alonzo.Transform.ANorm where

import Data.Maybe
import Data.Typeable (Typeable)
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Language.Alonzo.Transform.NameBind as B

type Var = Name Val

data Exp
  = EVal Val
  | EPrim PrimInstr Val Val
  | EApp Val [Val]
  | ELet (Bind (Rec [(Var, Embed Exp)]) Exp)
  | ELoc Loc Exp
  deriving(Show, Generic, Typeable)

data Val
  = VVar Var
  | VLam (Bind [Var] Exp)
  | VVal PrimVal
  | VLoc Loc Val
  | VWild
  deriving(Show, Generic, Typeable)


instance Alpha Exp
instance Alpha Val

instance Subst Val Loc
instance Subst Val Region
instance Subst Val Position
instance Subst Val PrimVal
instance Subst Val PrimInstr

instance Subst Val Exp
instance Subst Val Val where
  isvar (VVar x) = Just (SubstName x)
  isvar _ = Nothing


name :: B.Var -> Var
name = string2Name . name2String

lam :: [Var] -> Exp -> Exp
lam vs body =
  EVal . VLam $ bind vs body

let_ :: [(Var, Exp)] -> Exp -> Exp
let_ [] b = b
let_ bs b =
  let bs' = rec [ (v, embed t) | (v, t) <- bs ]
  in ELet $ bind bs' b


-- foo = \x y. f (g x) (h y)
--      = \x y. 
--        let x1 = g x
--            x2 = h y
--        in  f x1 x2

normalize :: B.Term -> Exp
normalize = runFreshM . anf

anf :: B.Term -> FreshM Exp
anf = \case
  B.TVar v -> return . EVal . VVar $ name v
  B.TVal v -> return . EVal $ VVal v
  
  B.TPrim i a b -> do
    ([a', b'], brs) <- anfVals [a, b]
    return $ let_ brs (EPrim i a' b')

  B.TApp f xs -> do
    (f':xs', brs) <- anfVals (f:xs)
    return $ let_ brs (EApp f' xs')

  B.TLam bs -> do
    (vs, t) <- unbind bs
    t' <- anf t
    return $ lam (name <$> vs) t' 

  B.TLet bs -> do
    (brs, body) <- unbind bs
    brs' <- mapM (\(v, t) -> anf (unembed t) >>= \t' -> return (name v, t')) (unrec brs)
    body' <- anf body
    return $ let_ brs' body'

  B.TLoc l t -> anf t >>= (return . ELoc l)
  B.TWild -> return $ EVal VWild


anfVals :: [B.Term] -> FreshM ([Val], [(Var, Exp)])
anfVals ts = do
  rs <- mapM anfVal ts
  let vs = fst <$> rs
      ds = catMaybes $ snd <$> rs
  return (vs, ds)

anfVal :: B.Term -> FreshM (Val, Maybe (Var, Exp))
anfVal tm = case tm of
  B.TVar v -> return (VVar (name v), Nothing)
  B.TVal v -> return (VVal v, Nothing)
  
  B.TLoc l t -> do
    (v, mb) <- anfVal t
    case mb of
      Nothing       -> return (VLoc l v, Nothing)
      Just (var, f) -> return (VLoc l v, Just (var, ELoc l f))

  B.TWild   -> return (VWild, Nothing)

  _ -> do
    v <- fresh $ string2Name "x"
    tm' <- anf tm
    return (VVar v, Just (v, tm'))


instance Pretty Exp where
  pretty = runFreshM . prettyFreshExp

instance Pretty Val where
  pretty = runFreshM . prettyFreshVal


prettyFreshExp :: Exp -> FreshM (Doc ann)
prettyFreshExp = \case
  EVal v -> prettyFreshVal v

  EPrim i t1 t2 -> do
    t1' <- prettyFreshVal t1
    t2' <- prettyFreshVal t2
    return $ pretty i <+> parens t1' <+> parens t2'
  
  EApp f as -> do
    f' <- prettyFreshVal f 
    as' <- mapM prettyFreshVal as
    return $ f' <+> hsep as'

  ELet bnd -> do
    (brs, body) <- unbind bnd
    brs' <- mapM (\(v, e) -> prettyFreshExp (unembed e) >>= \e' -> return (pretty (show v) <+> "=" <+> e')) (unrec brs)
    body' <- prettyFreshExp body
    return $ "let" <+> hsep (punctuate comma brs') <+> "in" <+> body'
  
  ELoc _ e -> prettyFreshExp e



prettyFreshVal :: Val -> FreshM (Doc ann)
prettyFreshVal = \case  
  VVar v        -> return . pretty . show $ v
  VLam bnd -> do
    (xs, body) <- unbind bnd
    let xs' = (pretty . show) <$> xs 
    body' <- prettyFreshExp body
    return $ "\\" <> hsep xs' <> "." <+> body'

  VVal v   -> return $ pretty v
  VLoc _ v -> prettyFreshVal v
  VWild    -> return "_"