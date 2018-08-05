module Language.Alonzo.Analysis.NameCheck where

import Data.Text (Text)
import Data.Set (Set)
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Syntax.Source

import qualified Data.Set as Set


data NCError
  = NameNotFound Text Loc

namecheck :: Set Text -> Term -> Maybe NCError
namecheck vs t = undefined