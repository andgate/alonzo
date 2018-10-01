{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Parse.Error where


import Data.Text.Prettyprint.Doc
import Language.Alonzo.Lex.Error
import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Source


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar [Stmt]
    | PLexErr LexError


instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        -- This should never happen.
        -- Please email me if you get this.
        AmbiguousGrammar cls ->
            vcat [ pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."
                 , vcat (pretty <$> cls)
                 ]

        PLexErr err ->
            pretty err