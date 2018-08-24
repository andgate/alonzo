{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
            , TemplateHaskell
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
import qualified Data.List.NonEmpty  as NE

-- -----------------------------------------------------------------------------
-- | Closure

data Closure = Closure [Program] [Term]

data Program =
  Program { _progName :: (Text, Loc)
          , _progTerm :: Term
          }


instance Semigroup Closure where
  (Closure ps1 ts1) <> (Closure ps2 ts2)
    = Closure (ps1 <> ps2) (ts1 <> ts2)

instance Monoid Closure where
  mempty = Closure mempty mempty
 

closure :: [Stmt] -> Closure
closure ss = foldr go mempty ss
  where
    go (SProg (n, l) t) cl = cl <> Closure [Program (n,l) t] []
    go (STerm t) cl = cl <> Closure [] [t] 

-- -----------------------------------------------------------------------------
-- | Statements

data Stmt
  = SProg (Text, Loc) Term
  | STerm Term


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
  
  -- Location decorator
  | TLoc    Loc Term
  | TParens Term
  | TWild
  deriving (Show, Generic, Typeable)


instance Locatable Term where
  locOf = \case
    -- We expect TLoc to wrap every parsed term
    TLoc l _ -> l
    _        -> error "Location not found!"

-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty Closure where
  pretty (Closure ps ts) =
    vsep $ [pretty n <+> "=" <+> pretty t | (Program (n, _) t) <- ps]
         ++ map pretty ts

instance Pretty Stmt where
  pretty = \case
    SProg (n, _) t -> pretty n <+> "=" <+> pretty t
    STerm t -> pretty t

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
      
      TLoc _ t  -> pretty t -- ignore location
      TParens t -> parens $ pretty t
      TWild -> "_"



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