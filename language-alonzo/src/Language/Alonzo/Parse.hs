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
import Text.Earley (Report (..), Prod)

import Language.Alonzo.Lex (lex)
import Language.Alonzo.Lex.LFCut
import Language.Alonzo.Lex.Token
import Language.Alonzo.Lex.Organize
import Language.Alonzo.Parse.Error
import Language.Alonzo.Parse.Grammar
import Language.Alonzo.Syntax.Source

import qualified Text.Earley as E


parseFile :: FilePath -> Text -> Either ParseError [Decl]
parseFile fp srcTxt =
  runExcept $ do
    toks <- withExcept PLexErr $ lex fp srcTxt
    mapM parse (lfCut $ organize toks)


parseDecl :: FilePath -> Text -> Either ParseError Decl
parseDecl fp srcTxt =
  runExcept $ do 
    toks <- withExcept PLexErr $ lex fp srcTxt
    parse toks


parse :: [Token] -> Except ParseError Decl
parse toks = do
  let (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser alonzoGrammar) toks
  case parses of
    []  -> throwError $ UnexpectedToken unconsumed expected
    [p] -> return p
               
    -- This will only happen if the grammar is wrong
    ps -> throwError $ AmbiguousGrammar ps