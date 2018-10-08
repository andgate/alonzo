{-# LANGUAGE  DeriveDataTypeable
            , DeriveGeneric
            , LambdaCase 
            #-}
module Language.Alonzo.Transform.Lift where


import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import GHC.Generics
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Builtin
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (Fold, toListOf)

import qualified Data.Map as Map
import qualified Language.Alonzo.Transform.ANF as A

--------------------------------------------------------------------------------------------------
-- Lambda Lifted Closure

type Closure = (Lambdas, [Term])

type Dependencies = Set String

type Lambdas = Map String (Bind [Var] Term)

data Lambda = Lambda String (Bind [Var] Term)
  deriving(Show, Generic, Typeable)


--------------------------------------------------------------------------------------------------
-- Term Definition

type Var = Name Term

data Term
  = TVar Var
  | TVal Val
  | TPrim PrimInstr Term Term
  | TApp Term [Term]
  | TLet (Bind (Rec [(Var, Embed Term)]) Term)
  | TLoc Loc Term
  | TWild
  deriving(Show, Generic, Typeable)


lift :: A.Closure -> FreshM Closure
lift cl = runFreshM $ undefined

liftTerm :: A.Term -> FreshM Term
liftTerm = \case
  A.TVal v -> undefined
  A.TPrim i a b -> TPrim i <$> liftVal a <*> liftVal b
  A.TApp f as -> undefined
  A.TLet bnd -> undefined
  A.TLoc l t -> TLoc l <$> liftTerm t


liftVal :: A.Val -> FreshM Term
liftVal = \case
  A.VVar v -> undefined
  A.VLam bnd -> undefined
  A.VVal v -> undefined
  A.VLoc l v -> TLoc l <$> liftVal v
  A.VWild -> return TWild
