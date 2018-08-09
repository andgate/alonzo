{-# LANGUAGE  DeriveDataTypeable
            , DeriveGeneric
            , LambdaCase 
            #-}
module Language.Alonzo.Transform.Lift where

import Data.Text
import Data.Typeable (Typeable)
import GHC.Generics
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Language.Alonzo.Transform.ANorm as A

type Var = Name Term

data Lambda = Lambda String (Bind [Var] Term)
  deriving(Show, Generic, Typeable)

data Term
  = TVar Var
  | TVal PrimVal
  | TPrim PrimInstr Term Term
  | TApp Term [Term]
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)
  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)


lift :: A.Term -> (Term, [Lambda])
lift t = undefined