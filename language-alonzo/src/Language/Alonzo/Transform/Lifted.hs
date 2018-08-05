{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Transform.Lifted where

import Data.Typeable (Typeable)
import GHC.Generics
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Language.Alonzo.Transform.ANorm as A

type Var = Name Term

data Lambda = Lambda Text [Var] Term

data Term
  = TVal
  | TPrim PrimInstr Term Term
  | TApp Term [Term]
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)
  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)