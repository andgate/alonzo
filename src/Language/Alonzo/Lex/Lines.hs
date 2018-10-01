{-# LANGUAGE LambdaCase #-}
module Language.Alonzo.Lex.Lines where

import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Location

-- | Split a list of tokens at toplevel linefolds
splitlf :: [Token] -> [[Token]]
splitlf = filter (not . null) . foldr go ([]:[])
  where
    go :: Token         -- Cut accumulator
       -> [[Token]]     -- Cuts
       -> [[Token]]
    go t (cut:cuts) = case t of
      Token _ _ (Loc _ (R (P _ 0) _))
        -> ([]:((t:cut):cuts))

      Token TokenEof _ _
        -> (cut:cuts)

      _ -> ((t:cut):cuts)
