{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Parse.Error where


import Data.Text.Prettyprint.Doc
import Language.Alonzo.Lex.Error
import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Source (Decl)


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar [Decl]
    | PLexErr LexError
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        AmbiguousGrammar srcs ->
            vcat [ pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."
                 , vcat (pretty <$> srcs)
                 ]

        PLexErr err ->
            pretty err