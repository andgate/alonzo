{-# LANGUAGE RecursiveDo
           , RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , TupleSections
  #-}
module Language.Alonzo.Parse.TopLevel where

import Control.Applicative hiding (optional)
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Text (Text, pack)

import Language.Alonzo.Parse.Decl
import Language.Alonzo.Parse.Helpers
import Language.Alonzo.Lex.Token (Token)
import Language.Alonzo.Syntax.Source
import Language.Alonzo.Syntax.Source.Helpers

import Text.Earley
import Text.Earley.Mixfix

import qualified Data.List.NonEmpty as NE


-- -----------------------------------------------------------------------------
-- Grammar for Alonzo
toplevelGrammar :: Grammar r (Prod r String Token TopLevelDecl)
toplevelGrammar = mdo

    toplevel <- rule $
      modDecl <|> classDecl

    modDecl <- rule $
      let ex (n, _) ds = ModuleDecl n ds
      in ex <$> (rsvp "module" *> conId) <*> (rsvp ":" *> block declGrammar)

    classDecl <- rule $
      let ex (n, _) vs = ModuleDecl n (fst <$> ds)
      in ex <$> (rsvp "class" *> conId) <*> many varId


    return toplevel