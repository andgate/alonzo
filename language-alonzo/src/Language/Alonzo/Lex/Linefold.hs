{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Lex.Linefold where

import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Location

-- | Split a list of tokens at toplevel linefolds
splitlf :: [Token] -> [[Token]]
splitlf = splitlf' [] []

splitlf' :: [Token] -> [[Token]] -> [Token] -> [[Token]]
splitlf' cur cut = \case 
  [] ->
      tail $ reverse $ (reverse cur):cut
  
  (t@(Token _ _ (Loc _ (R (P _ 0) _))):ts) ->
      splitlf' [t] ((reverse cur):cut) ts

  ((Token TokenEof _ _):ts) ->
      splitlf' cur cut ts
  
  (t:ts) -> 
      splitlf' (t:cur) cut ts