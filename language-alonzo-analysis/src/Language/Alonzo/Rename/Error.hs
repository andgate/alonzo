{-# Language OverloadedStrings
           , LambdaCase
  #-}
module Language.Alonzo.Rename.Error where


import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import Language.Alonzo.Syntax.Location


data RenameError
  = UndeclaredName Loc Text
    deriving(Show)

instance Pretty RenameError where
    pretty = \case
        UndeclaredName l n ->
            "Unrecognizd Name:" <+> pretty n