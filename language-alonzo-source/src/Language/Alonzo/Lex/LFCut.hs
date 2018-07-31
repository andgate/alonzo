module Language.Alonzo.Lex.LFCut where

import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Location

-- | Split a list of tokens at toplevel linefolds
lfCut :: [Token] -> [[Token]]
lfCut = lfCut' [] []

lfCut' :: [Token] -> [[Token]] -> [Token] -> [[Token]]
lfCut' cur cut [] = tail $ reverse $ (reverse cur):cut
lfCut' cur cut (t@(Token TokenLn _ (Loc _ (R (P _ 0) _))):ts) = 
  lfCut' [t] ((reverse cur):cut) ts
lfCut' cur cut ((Token TokenEof _ _):ts) = 
  lfCut' cur cut ts
lfCut' cur cut (t:ts) = 
  lfCut' (t:cur) cut ts