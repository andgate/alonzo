{-# Language DeriveDataTypeable
           , DeriveGeneric
           , MultiParamTypeClasses
           , OverloadedStrings
           , LambdaCase
           , RankNTypes
  #-}
module Language.Alonzo.Syntax.Bound where

import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import Language.Alonzo.Syntax.Prim
import Language.Alonzo.Syntax.Location


--------------------------------------------------------------------------------------------------
-- Term Definition


data Fun = Fun String Term
  deriving(Show, Generic, Typeable)

type Var = Name Term

data Term
  = TVar Var
  | TVal PrimVal
  | TPrim PrimInstr Term Term

  | TApp Term [Term]
  | TLam (Bind [Var] Term)
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)

  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Smart Constructors

tvar :: String -> Term
tvar = TVar . string2Name


tlam :: [String] -> Term -> Term
tlam xs b = TLam $ bind (string2Name <$> xs) b


tapp :: Term -> Term -> Term
tapp f x = TApp f [x]

tapps :: Term -> [Term] -> Term
tapps = TApp


tlet :: [(String, Term)] -> Term -> Term
tlet bs b = 
  let bs' = rec [ (string2Name v, embed t) | (v, t) <- bs]
  in TLet $ bind bs' b


free :: Term -> [Text]
free t =
  (pack . name2String) <$> toListOf (fv :: Fold Term (Name Term)) t

------------------------------------------------------------------------------------------------------
-- Instances Required for unbound-generics

instance Alpha Term

instance Alpha Loc where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Region where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha Position where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha PrimInstr where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ

instance Alpha PrimVal where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ


instance Subst Term Loc
instance Subst Term Region
instance Subst Term Position
instance Subst Term PrimInstr
instance Subst Term PrimVal

instance Subst Term Term where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing



instance Pretty Term where
  pretty = runFreshM . prettyFresh



prettyFresh :: Term -> FreshM (Doc ann)
prettyFresh = \case
  TVar v        -> return . pretty . show $ v
  TVal v        -> return $ pretty v
  TPrim i t1 t2 -> do
    t1' <- prettyFresh t1
    t2' <- prettyFresh t2
    return $ pretty i <+> parens t1' <+> parens t2'
  
  TApp f as -> do
    f' <- prettyFresh f 
    as' <- mapM prettyFresh as
    return $ f' <+> hcat as'

  TLam bnd -> do
    (xs, body) <- unbind bnd
    let xs' = (pretty . show) <$> xs 
    body' <- prettyFresh body
    return $ "\\" <+> hcat xs' <+> "." <+> body'

  TLet bnd -> undefined

  TLoc _ t -> prettyFresh t
  TWild -> return "_"