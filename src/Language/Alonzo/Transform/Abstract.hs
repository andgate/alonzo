{-# Language DeriveDataTypeable
           , DeriveGeneric
           , ExistentialQuantification
           , GADTs
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , MultiParamTypeClasses
           , OverloadedStrings
           , LambdaCase
           , RankNTypes
           , ScopedTypeVariables
           , TemplateHaskell
  #-}
module Language.Alonzo.Transform.Abstract where

import Control.Lens hiding (Fold, toListOf)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Data.Typeable (Typeable)
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Builtin

import qualified Data.Map.Strict                as Map
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as Set
import qualified Language.Alonzo.Syntax.Source  as S


--------------------------------------------------------------------------------------------------
-- Name Bound Closure

data Closure = Closure (Bind (Rec [(Var, Embed Term)]) [Term])
  deriving(Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Term Definition

type Var = Name Term

data Term
  = TVar Var
  | TVal Val
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

instance Alpha Val where
  aeq' c s1 s2 = True
  acompare' c s1 s2 = EQ


instance Subst Term Loc
instance Subst Term Region
instance Subst Term Position
instance Subst Term PrimInstr
instance Subst Term Val

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
    return $ f' <+> hsep as'

  TLam bnd -> do
    (xs, body) <- unbind bnd
    let xs' = (pretty . show) <$> xs
    body' <- prettyFresh body
    return $ "\\" <> hsep xs' <> "." <+> body'

  TLet bnd -> do
    (brs, body) <- unbind bnd
    brs' <- mapM (\(v, t) -> prettyFresh (unembed t) >>= \t' -> return (pretty (show v) <+> "=" <+> t')) (unrec brs)
    body' <- prettyFresh body
    return $ "let" <+> hsep brs' <+> "in" <+> body'

  TLoc _ t -> prettyFresh t
  TWild -> return "_"


------------------------------------------------------------------------------------------------------
-- Name Binding

{-
namebind :: S.Closure -> Closure
namebind (S.Closure ps ts) = Closure $ bind (rec ps') ts'
  where
    ts' = namebindTerm <$> ts
    ps' = [ (string2Name (unpack n), embed (namebindTerm t))
          | (S.Program (n, _) t) <- ps
          ]

namebindTerm :: S.Term -> Term
namebindTerm = \case
  S.TVar (n, _) -> tvar $ unpack n

  S.TVal v    -> TVal v

  S.TPrim i t1 t2 -> TPrim i (namebindTerm t1) (namebindTerm t2)

  S.TApp f as ->
    tapps (namebindTerm f) (namebindTerm <$> NE.toList as)

  S.TLam vs body ->
    let vs' = unpack . fst <$> NE.toList vs
        body' = namebindTerm body
    in tlam vs' body'

  S.TLet fns body ->
    tlet (renameFuns $ NE.toList fns) (namebindTerm body)

  S.TLoc l t   -> TLoc l (namebindTerm t)
  S.TParens t  -> namebindTerm t
  S.TWild      -> TWild


renameFuns :: [((Text, Loc), S.Term)] -> [(String, Term)]
renameFuns = map renameFun

renameFun :: ((Text, Loc), S.Term) -> (String, Term)
renameFun ((n, _), t) = (unpack n, namebindTerm t)
-}
