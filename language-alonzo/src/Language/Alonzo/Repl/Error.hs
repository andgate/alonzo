{-# Language LambdaCase #-}
module Language.Alonzo.Repl.Error where

import Data.Text.Prettyprint.Doc
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse.Error
import Language.Alonzo.Analysis.Error


data ReplError
  = ReplParseErr ParseError
  | ReplAnalysisErr AnalysisError
  | ReplLexErr LexError

instance Pretty ReplError where
    pretty = \case
        ReplParseErr err -> pretty err
        ReplAnalysisErr err -> pretty err
        ReplLexErr err -> pretty err