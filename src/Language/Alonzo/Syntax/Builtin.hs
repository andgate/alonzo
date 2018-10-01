{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
module Language.Alonzo.Syntax.Builtin where

import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)


-- -----------------------------------------------------------------------------
-- | Type

data Val
  = VInt Integer
  | VFloat Double
  | VChar Char
  | VString String
  | VBool Bool
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)


data PrimInstr
  = PrimAdd
  | PrimFAdd
  | PrimSub
  | PrimFSub
  | PrimMul
  | PrimFMul
  | PrimDiv
  | PrimUDiv
  | PrimSDiv
  | PrimFDiv
  | PrimEq
  | PrimLt
  | PrimLtEq
  | PrimGt
  | PrimtGtEq
  | PrimNEq
  | PrimNLt
  | PrimNLtEq
  | PrimNGt
  | PrimNGtEq
  | PrimBad
  deriving (Read, Show, Eq, Ord, Enum, Data, Typeable, Generic)


intInstrs :: [PrimInstr]
intInstrs = 
  [ PrimAdd
  , PrimSub
  , PrimMul
  , PrimDiv
  , PrimUDiv
  , PrimSDiv
  ]


floatInstrs :: [PrimInstr]
floatInstrs =
  [ PrimFAdd
  , PrimFSub
  , PrimFMul
  , PrimFDiv
  , PrimFDiv
  ]


-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------


instance Pretty Val where
  pretty = \case
    VInt v ->
      pretty v

    VFloat v ->
      pretty v

    VChar c ->
      squotes $ pretty c

    VString v ->
      pretty v

    VBool v ->
      pretty v


instance Pretty PrimInstr where
    pretty =
      pretty . pack . show


-- Serialize ---------------------------------------------------------------------

instance Binary Val
instance FromJSON Val
instance ToJSON Val

instance Binary PrimInstr
instance FromJSON PrimInstr
instance ToJSON PrimInstr


-- String

readPrimInstr :: Text -> PrimInstr
readPrimInstr = \case
  "#add"   -> PrimAdd
  "#fadd"  -> PrimFAdd
  "#sub"   -> PrimSub
  "#fsub"  -> PrimFSub
  "#mul"   -> PrimMul
  "#fmul"  -> PrimFMul
  "#div"   -> PrimDiv
  "#udiv"  -> PrimUDiv
  "#sdiv"  -> PrimSDiv
  "#fdiv"  -> PrimFDiv
  "#eq"    -> PrimEq
  "#lt"    -> PrimLt
  "#lteq"  -> PrimLtEq
  "#gt"    -> PrimGt
  "#gteq"  -> PrimtGtEq
  "#neq"   -> PrimNEq
  "#nlt"   -> PrimNLt
  "#nlteq" -> PrimNLtEq
  "#ngt"   -> PrimNGt
  "#ngteq" -> PrimNGtEq
  _ -> PrimBad




evalInstr :: (PrimInstr, Val, Val) -> Val
evalInstr = \case
  (PrimAdd, VInt x, VInt y) -> VInt (x + y)
  (PrimFAdd, VFloat x, VFloat y) -> VFloat (x + y)
  (PrimSub, VInt x, VInt y) -> VInt (x - y)
  (PrimFSub, VFloat x, VFloat y) -> VFloat (x - y)
  (PrimMul, VInt x, VInt y) -> VInt (x * y)
  (PrimFMul, VFloat x, VFloat y) -> VFloat (x * y)
  (PrimDiv, VInt x, VInt y) -> VInt (div x y)
  (PrimFDiv, VFloat x, VFloat y) -> VFloat (x / y)
  (PrimEq, VInt x, VInt y) -> VBool (x == y)
  (PrimLt, VInt x, VInt y) -> VBool (x <= y)
  (PrimLtEq, VInt x, VInt y) -> VBool (x <= y)
  (PrimGt, VInt x, VInt y) -> VBool (x > y)
  (PrimtGtEq, VInt x, VInt y) -> VBool (x >= y)
  (PrimNEq, VInt x, VInt y) -> VBool (not $ x == y)
  (PrimNLt, VInt x, VInt y) -> VBool (not $ x < y)
  (PrimNLtEq, VInt x, VInt y) -> VBool (not $ x <= y)
  (PrimNGt, VInt x, VInt y) -> VBool (not $ x > y)
  (PrimNGtEq, VInt x, VInt y) -> VBool (not $ x >= y)
  _ -> error "Unsupported operation"
