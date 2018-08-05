{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , GADTs
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , ExistentialQuantification
           , TemplateHaskell
  #-}
module Language.Alonzo.Transform.Rename where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Set (Set)
import Data.Text (Text, unpack)
import Language.Alonzo.Rename.Error
import Language.Alonzo.Syntax.Bound
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Prim
import Unbound.Generics.LocallyNameless


import qualified Data.Set as Set
import qualified Language.Alonzo.Syntax.Source as S


import qualified Data.List.NonEmpty             as NE