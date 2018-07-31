{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
  #-}
module Language.Alonzo.Syntax.Source
  ( module Language.Alonzo.Syntax.Source
  , module X
  )
  where

import GHC.Generics
import Data.Typeable (Typeable)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Maybe

import Language.Alonzo.Syntax.Location as X
import Language.Alonzo.Syntax.Name as X
import Language.Alonzo.Syntax.Prim as X

import qualified Data.List.NonEmpty             as NE


-- -----------------------------------------------------------------------------
-- | Top Level Declarations

data Decl
  = ModDecl  Text
  | TermDecl Term
  | FunDecl  Fun
  | DataDecl Data
  deriving (Show, Generic, Typeable)


-- -----------------------------------------------------------------------------
-- | Definitions

data Fun
  = Fun Text [Text] Term
  deriving (Show, Generic, Typeable)

-- -----------------------------------------------------------------------------
-- | Term

data Term
  -- x
  = TVar  Text 
  -- A
  | TCon  Text
  -- integers, floats, characters, booleans, etc.
  | TVal  PrimVal
  
  -- #add, #sub, #eq, etc.
  | TPrim PrimInstr Term Term
  
  -- f a
  | TApp   Term (NonEmpty Term)
  
  -- Lambda types represent term functions
  -- λ x . body
  -- \ x . body
  -- forall x . body
  | TLam   (NonEmpty Pat) Term

  -- let x = t in body
  | TLet   (NonEmpty PatBind) Term
  | TCase  Term (NonEmpty PatBind)
  
  -- Location decorator
  | TLoc    Loc Term
  | TParens Term
  | TWild
  deriving (Show, Generic, Typeable)


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar Text
  | PWild
  | PParen Pat
  | PLoc Loc Pat
  deriving (Show, Generic, Typeable)


data PatBind = PatBind Pat Term
  deriving (Show, Generic, Typeable)


instance Locatable Term where
  locOf = \case
    -- Usually, we only want a top level location
    TLoc l _ -> l
    _        -> error "Location not found!"

pvarFind' = fromJust . pvarFind

pvarFind :: Pat -> Maybe Text
pvarFind = \case
  PVar v   -> Just v
  PWild    -> Nothing
  PParen p -> pvarFind p
  PLoc l p -> pvarFind p


------------------------------------------------------------------------
-- Data Definition

data Data
  = Data Text [Constr]
  deriving (Show, Generic, Typeable)


data Constr
  = Constr Text [Text]
  deriving (Show, Generic, Typeable)




-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty Decl where
  pretty = \case
    ModDecl         n -> "module" <+> pretty n
    TermDecl        x -> pretty x
    FunDecl         x -> pretty x
    DataDecl        x -> pretty x

instance Pretty Fun where
  pretty (Fun n xs body) =
    pretty n <+> hcat (pretty <$> xs) <+> "=" <+> pretty body


instance Pretty Term where
    pretty = \case
      -- Terms
      TVar n      -> pretty n
      TCon n      -> pretty n
      TVal v      -> pretty v
      
      TPrim i t t' -> pretty (show i) <+> pretty t <+> pretty t'
      
      TApp e1 e2  -> pretty e1 <+> hcat (pretty <$> NE.toList e2)
      
      TLam ps body -> "\\" <+> hcat (pretty <$> NE.toList ps) <+> "." <+> pretty body 

      TLet vs body -> "let" <+> nest 2 (vcat (pretty <$> NE.toList vs)) <+> pretty body
      TCase t brs  -> "case" <+> pretty t <+> "of" <+> nest 2 (vcat [pretty p <+> "->" <+> pretty t | (PatBind p t) <- NE.toList brs])
      
      TLoc _ t  -> pretty t -- ignore location
      TParens t -> parens $ pretty t
      TWild -> "_"


instance Pretty Pat where
  pretty = \case
    PVar x -> pretty x
    PWild -> "_"
    PParen p  -> parens $ pretty p
    PLoc _ p -> pretty p -- omit location

instance Pretty PatBind where
  pretty (PatBind p t) = pretty p <+> "=" <+> pretty t


instance Pretty Data where
    pretty (Data n cs) =
      pretty n <+> hang 0 (":=" <+> hsep (punctuate (line <> pipe) (pretty <$> cs)))


instance Pretty Constr where
    pretty (Constr n fs) =
        pretty n <+> hcat (pretty <$> fs)

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


