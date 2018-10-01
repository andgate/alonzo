{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
            , TemplateHaskell
            , RecordWildCards
  #-}
module Language.Alonzo.Syntax.Source
  ( module Language.Alonzo.Syntax.Source
  , module X
  )
  where

import Control.Lens
import GHC.Generics
import Data.Typeable (Typeable)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup

import Language.Alonzo.Syntax.Location as X
import Language.Alonzo.Syntax.Prim as X

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NE


-- -----------------------------------------------------------------------------
-- | Names

data Name
  = Name
    { nameText :: Text
    , nameLoc :: Loc
    }
  deriving (Show, Generic, Typeable)


data QName
  = QName
    { qnameText :: Text
    , qnamePath :: [Text]
    , qnameLoc :: Loc
    }
  deriving (Show, Generic, Typeable)


mkName :: L Text -> Name
mkName (n, l) =
  Name { nameText = n, nameLoc = l }


mkQName :: L Text -> QName
mkQName (n, l) = case T.splitOn "." n of
  []     -> error "Empty name encountered"
  (n:[]) -> QName { qnameText = n, qnamePath = [], qnameLoc = l }
  ns     -> QName { qnameText = last n, qnamePath = init n, qnameLoc = l }


-- -----------------------------------------------------------------------------
-- | Module

data Module
  = Module
    { modName    :: QName
    , modImports :: [QName]
    , modData    :: [DataDef]
    , modFns     :: [Term]
    }


-- -----------------------------------------------------------------------------
-- | Functions

data Func
  = Func { fnName   :: Name
         , fnClause :: Clause
         }

data Clause =
  Clause [Pat] Term 


-- -----------------------------------------------------------------------------
-- | Statements

data Stmt
  = SImport QName
  | SFunc Func
  | STerm Term
  | SData DataDef


mkModule :: QName -> [Stmt] -> Module
mkModule =
  where
    mod = Module { modName = }

-- -----------------------------------------------------------------------------
-- | Data Definition

data DataDef =
  DataDef Name [Name]


-- -----------------------------------------------------------------------------
-- | Term

data Term
  -- x
  = TVar  (Text, Loc)
  -- integers, floats, characters, booleans, etc.
  | TVal  PrimVal

  -- f a
  | TApp   Term (NonEmpty Term)

  -- #add, #sub, #eq, etc.
  | TPrim PrimInstr Term Term

  -- Lambda types represent term functions
  -- λ x . body
  -- \ x . body
  -- forall x . body
  | TLam   (NonEmpty (Text, Loc)) Term

  -- let x = t in body
  | TLet   (NonEmpty ((Text, Loc), Term)) Term

  | TCase Term (NonEmpty (Pat, Term))

  -- Location decorator
  | TLoc    Loc Term
  | TParens Term
  | TWild
  deriving (Show, Generic, Typeable)


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar Name
  | PVal PrimVal
  | PCon Name [Pat]
  | PAs Name Pat
  | PParens Pat
  | PLoc Loc Pat
  | PWild
  deriving (Show, Generic, Typeable)


-- -----------------------------------------------------------------------------
-- | Locatable Instances

instance Locatable Term where
  locOf = \case
    -- We expect TLoc to wrap every parsed term
    TLoc l _ -> l
    _        -> error "Location not found!"

instance Locatable Pat where
  locOf = \case
    -- We expect TLoc to wrap every parsed term
    PLoc l _ -> l
    _        -> error "Location not found!"


-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty Name where
  pretty Name{..} =
    pretty nameText

instance Pretty QName where
  pretty QName{..} =
    mconcat $ punctuate dot (pretty <$> NE.toList qnameText)

instance Pretty Module where
  pretty Module{..} =
    vsep $  ["module" <+> pretty modName]
         ++ ["import" <+> pretty qn | qn <- modImports]
         ++ map pretty modData
         ++ map pretty modFns

instance Pretty Func where
  pretty Func{..} =
    pretty fnName <+> pretty fnClause

instance Pretty Clause where
  pretty (Clause ps body) =
    hsep (pretty <$> ps) <+> "=" <> line <> nest 4 (pretty body)


instance Pretty Stmt where
  pretty = \case
    SMod qn    -> "module" <+> pretty qn
    SImport qn -> "import" <+> pretty qn
    SFunc fn   -> pretty fn
    STerm t    -> pretty t
    SData d    -> pretty d
    

instance Pretty DataDef where
  pretty (DataDef n fs) =
    pretty n <> ":" <+> hsep (pretty <$> fs)
    

instance Pretty Term where
    pretty = \case
      -- Terms
      TVar (n, _) -> pretty n
      TVal v      -> pretty v

      TPrim i t t' -> pretty (show i) <+> pretty t <+> pretty t'

      TApp e1 e2  -> pretty e1 <+> hsep (pretty <$> NE.toList e2)

      TLam ps body -> "\\" <+> hsep (pretty . fst <$> NE.toList ps) <+> "." <+> pretty body 

      TLet bs body ->
          "let" <+> hsep (punctuate comma [pretty x <+> "=" <+> pretty t | ((x, _), t) <- NE.toList bs]) <+> "in" <+> pretty body

      TCase t brs ->
        "case" <+> pretty t <+> "of" <> line
          <> nest 2 (hsep [pretty p <+> "->" <+> pretty t | (p, t) <- NE.toList brs])

      TLoc _ t  -> pretty t -- ignore location
      TParens t -> parens $ pretty t
      TWild -> "_"


instance Pretty Pat where
  pretty = \case
      -- Terms
      PVar n      -> pretty n
      PVal v      -> pretty v
      PCon n ps   -> pretty n <+> hsep (pretty <$> ps)
      PAs n p     -> pretty n <> "@" <> parens (pretty p)
      PLoc _ p    -> pretty p -- ignore location
      PParens p   -> parens $ pretty p
      PWild       -> "_"


{-
instance Pretty Fixity where
  pretty (Fixity f p ops) =
    vcat
      [ "Fixity Declaration"
      , indent 2 $ vcat
        [ "Fixity:" <+> pretty (pack $ show f)
        , "Precedence:" <+> pretty (pack $ show p)
        , "Operators:" <+> pretty ops
        ]
      ]


instance Pretty Foreign where
  pretty = \case
    ForeignImport ft fn hn ty ->
      vcat
        [ "Foreign Import:"
        , indent 2 $ vcat
            [ "Foreign Type:" <+> pretty ft
            , "Foreign Name:" <+> pretty fn
            , "Hawk Name:"    <+> pretty hn
            , "Type Sig:"     <+> pretty ty
            ]
        ]

    ForeignExport ft hn ->
      vcat
        [ "Foreign Export:"
        , indent 2 $ vcat
          [ "Foreign Type:" <+> pretty ft
          , "Hawk Name:"    <+> pretty hn
          ]
        ]

instance Pretty ForeignType where
  pretty ForeignC =
    "ForeignC"
-}
