{-# Language LambdaCase, OverloadedStrings #-}
module Language.Alonzo.Repl.Error where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse.Error
import Language.Alonzo.Analysis.Error


data ReplError
  = ReplParseErr
  | ReplAnalysisErr
  | ReplFileLoadErr FilePath
  | ReplSeriousErr Text -- Rly srs

instance Pretty ReplError where
    pretty = \case
        ReplParseErr -> "Parse Error!"
        ReplAnalysisErr -> "Analysis Error!"
        ReplFileLoadErr fp -> pretty fp <+> ": Failed to load."
        ReplSeriousErr err -> pretty err