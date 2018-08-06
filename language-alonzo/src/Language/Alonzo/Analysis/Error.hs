{-# Language OverloadedStrings
           , LambdaCase
  #-}
module Language.Alonzo.Analysis.Error where


import Data.Text.Prettyprint.Doc
import Data.Text (Text, pack)
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Analysis.NameCheck


data AnalysisError
  = ANameError NameError
  deriving(Show)

instance Pretty AnalysisError where
    pretty = \case
      ANameError e -> pretty e 