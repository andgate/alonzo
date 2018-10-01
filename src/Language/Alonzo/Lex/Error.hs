{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Lex.Error where

import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Location
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc


data LexError
  = UnproducibleToken String Loc
  | InvalidCharLit Text
  | IllegalLexerSkip
  deriving(Show)


instance Pretty LexError where
    pretty = \case
      UnproducibleToken cs l  ->
          pretty "Lexer has failed on"
            <+> dquotes (pretty cs)
            <+> pretty "at"
            <+> pretty l

      IllegalLexerSkip  ->
          pretty "Lexer performed an illegal skip operation."