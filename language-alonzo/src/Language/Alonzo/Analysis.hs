module Language.Alonzo.Analysis where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import Language.Alonzo.Analysis.Error
import Language.Alonzo.Analysis.Infer
import Language.Alonzo.Analysis.NameCheck
import Language.Alonzo.Syntax.Source


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

analysis :: Map Text Term -> [AnalysisError]
analysis cl = 
  let vs = Set.fromList (Map.keys cl)
      ts = Map.elems cl
      nameErrs = mconcat . map (\t -> namecheck vs (locOf t) t) $ ts
  in (ANameError <$> nameErrs)