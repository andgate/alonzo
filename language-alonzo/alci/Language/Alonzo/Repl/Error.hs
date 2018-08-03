{-# Language LambdaCase #-}
module Language.Alonzo.Repl.Error where

import Data.Text.Prettyprint.Doc
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse.Error
import Language.Alonzo.Rename.Error


data ReplError
  = ReplParseErr ParseError
  | ReplRenameErr RenameError
  | ReplLexErr LexError

instance Pretty ReplError where
    pretty = \case
        ReplParseErr err -> pretty err
        ReplRenameErr err -> pretty err
        ReplLexErr err -> pretty err