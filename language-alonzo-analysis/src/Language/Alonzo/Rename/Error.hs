{-# Language OverloadedStrings
           , LambdaCase
  #-}
module Language.Alonzo.Rename.Error where


import Data.Text.Prettyprint.Doc
import Data.Text (Text, pack)
import Language.Alonzo.Syntax.Location


data RenameError
  = UndeclaredConstr Loc String
  | UnknownGlobals Loc [String]
  | NameCollision Loc [String]
  deriving(Show)

instance Pretty RenameError where
    pretty = \case
        UndeclaredConstr l n ->
            pretty l <+> "Unrecognized Constructor:" <+> pretty n
        
        NameCollision l ns ->
            pretty l <+> "Name Collisions:" <+> hsep (pretty <$> ns)