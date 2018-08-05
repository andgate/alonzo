{-# Language DeriveDataTypeable
           , DeriveGeneric
           , MultiParamTypeClasses
           , OverloadedStrings
           , LambdaCase
           , RankNTypes
  #-}
module Language.Alonzo.Transform.Bound where

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
    return $ "\\" <> hsep xs' <> "." <+> body'

  TLet bnd -> undefined

  TLoc _ t -> prettyFresh t
  TWild -> return "_"


------------------------------------------------------------------------------------------------------
-- Instances Required for unbound-generics

data REnv = REnv
  { _rnLoc     :: Loc
  , _rnGlobals :: Set String
  }

makeLenses  ''REnv

newREnv :: [String] -> Loc -> REnv
newREnv vs l 
  = REnv { _rnLoc = l
         , _rnGlobals = Set.fromList vs
         }

newtype Renamer a = Renamer { unRenamer :: ReaderT REnv (Except RenameError) a }
  deriving (Functor, Applicative, Monad, MonadReader REnv, MonadError RenameError)


rename :: [String] -> S.Term -> Either RenameError Term
rename vs t = runRenamer (newREnv vs (locOf t)) $ renameTerm t


runRenamer :: REnv -> Renamer a -> Either RenameError a
runRenamer env rn = runExcept $ runReaderT (unRenamer rn) env

renameTerm :: S.Term -> Renamer Term
renameTerm = \case

  S.TVar n    -> return . tvar $ unpack n

  S.TVal v    -> return $ TVal v
  
  S.TPrim i t1 t2 -> TPrim i <$> renameTerm t1 <*> renameTerm t2

  S.TApp f as -> 
    tapps <$> renameTerm f <*> traverse renameTerm (NE.toList as)

  S.TLam vs body -> do
    let vs' = unpack <$> NE.toList vs
    chkDups vs'
    body' <- renameTerm body
    return $ tlam vs' body'

  S.TLet fns body ->
    tlet <$> renameFuns (NE.toList fns) <*> renameTerm body

  S.TLoc l t   -> TLoc l <$> local (rnLoc .~ l) (renameTerm t)
  S.TParens t  -> renameTerm t
  S.TWild      -> return TWild


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

renameFuns :: [(Text, S.Term)] -> Renamer [(String, Term)]
renameFuns fns =
  traverse renameFun fns 

renameFun :: (Text, S.Term) -> Renamer (String, Term)
renameFun (n, t) = do
  t' <- renameTerm t
  return (unpack n, t')