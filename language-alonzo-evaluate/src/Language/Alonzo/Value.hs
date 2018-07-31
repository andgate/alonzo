{-# Language LambdaCase, OverloadedStrings #-}
module Language.Alonzo.Value where

import Data.Text (Text)
import Language.Alonzo.Syntax.Bound
import Language.Alonzo.Syntax.Prim
import Data.Text.Prettyprint.Doc


data Value
  = VType
  | VLinear
  | VLam Text Term
  | VCon Text
  | VPrim PrimVal
  | VConst Const
  | VError String -- something bad happened!!
  deriving (Show)


data Const
  = CApp Const Value
  | CVar String
  deriving (Show)



instance Pretty Value where
  pretty = \case
    VType         -> "Type"
    VLinear       -> "Linear"
    VCon n        -> pretty n
    VPrim v       -> pretty v
    VConst v      -> pretty v
    VError err    -> pretty err


instance Pretty Const where
  pretty = \case
    CApp c v -> pretty c <+> pretty v
    CVar v   -> pretty v