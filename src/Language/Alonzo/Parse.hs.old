{-# LANGUAGE  FlexibleContexts 
            , TypeFamilies 
            , GeneralizedNewtypeDeriving
            , FlexibleInstances
            , MultiParamTypeClasses
            , TupleSections
            , LambdaCase
            , OverloadedStrings
  #-}
module Language.Alonzo.Parse where

import Prelude hiding (lex)

import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Earley (Report (..), Prod)

import Language.Alonzo.Lex (lex)
import Language.Alonzo.Lex.Linefold
import Language.Alonzo.Lex.Token
import Language.Alonzo.Parse.Error
import Language.Alonzo.Parse.Grammar
import Language.Alonzo.Syntax.Source

import qualified Text.Earley as E



parseFiles :: [(FilePath, Text)] -> ([ParseError], [[Stmt]])
parseFiles = foldr (\(es, ss) (es', ss') -> (es ++ es', ss:ss')) ([],[]) . map (uncurry parseText)

parseText :: FilePath -> Text -> ([ParseError], [Stmt])
parseText fp srcTxt =
    let toks = runExcept . withExcept PLexErr $ lex fp srcTxt
    in case splitlf <$> toks of
        Left e      -> ([e], [])
        Right toks' ->  partitionEithers $ map (runExcept . parseTokens) toks'


parseTokens :: [Token] -> Except ParseError Stmt
parseTokens toks = do
  let (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser alonzoGrammar) toks
  case parses of
    []  -> throwError $ UnexpectedToken unconsumed expected
    [p] -> return p
               
    -- This will only happen if the grammar is wrong
    ps -> throwError $ AmbiguousGrammar ps