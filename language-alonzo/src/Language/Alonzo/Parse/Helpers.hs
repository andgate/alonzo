{-# LANGUAGE  OverloadedStrings
            , ConstraintKinds
            , FlexibleContexts
            , FlexibleInstances
            , TypeFamilies
            , RankNTypes
  #-}
module Language.Alonzo.Parse.Helpers where

import Control.Applicative
import Control.Monad (void)
import Data.Monoid
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Alonzo.Lex.Token
import Language.Alonzo.Syntax.Source
import Text.Earley


-- -----------------------------------------------------------------------------
-- Terminal Production Helpers

match :: TokenClass -> Prod r e Token (Token, Loc)
match c = extract <$> satisfy p
  where p (Token c' _ _) = c == c'
        extract t@(Token _ _ l) = (t, l)

rsvp :: Text -> Prod r e Token (Token, Loc)
rsvp =
  match . TokenRsvp

optional :: Prod r e Token a -> Prod r e Token (Maybe a)
optional p = (Just <$> p) <|> pure Nothing


-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens
varId' :: Prod r e Token Text
varId' = fst <$> varId

varId :: Prod r e Token (Text, Loc)
varId = unsafeExtract <$> satisfy p
  where
    p (Token (TokenVarId _) _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenVarId n) _ l) = (n, l)


conId :: Prod r e Token (Text, Loc)
conId = unsafeExtract <$> satisfy p
  where
    p (Token (TokenConId _) _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenConId n) _ l) = (n, l)


primId :: Prod r e Token (Text, Loc)
primId = unsafeExtract <$> satisfy p
  where
    p (Token (TokenPrimId _) _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenPrimId n) _ l) = (n, l)


-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens

intLit :: Prod r e Token (Integer, Loc)
intLit = unsafeExtract <$> satisfy p
  where
    p (Token (TokenInteger _) _ _) = True
    p  _                           = False
    unsafeExtract (Token (TokenInteger v) _ l) = (v, l)

floatLit :: Prod r e Token (Double, Loc)
floatLit = unsafeExtract <$> satisfy p
  where
    p (Token (TokenDouble _) _ _) = True
    p  _                          = False
    unsafeExtract (Token (TokenDouble v) _ l) = (v, l)

charLit :: Prod r e Token (Char, Loc)
charLit = unsafeExtract <$> satisfy p
  where
    p (Token (TokenChar _) _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenChar v) _ l) = (v, l)

strLit :: Prod r e Token (String, Loc)
strLit = unsafeExtract <$> satisfy p
  where
    p (Token (TokenString _) _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenString v) _ l) = (v, l)

boolLit :: Prod r e Token (Bool, Loc)
boolLit = unsafeExtract <$> satisfy p
  where
    p (Token (TokenBool _) _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenBool v) _ l) = (v, l)


-- -----------------------------------------------------------------------------
-- Layout Helpers

eof :: Prod r e Token (Token, Loc)
eof = match TokenEof

block0 :: Prod r e Token a -> Prod r e Token [a]
block0 p = blk *> linefolds0 p <* blk'

block :: Prod r e Token a -> Prod r e Token [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: Prod r e Token a -> Prod r e Token [a]
linefolds0 p = many $ linefold p

linefolds :: Prod r e Token a -> Prod r e Token [a]
linefolds p = some $ linefold p

linefold :: Prod r e Token a -> Prod r e Token a
linefold p = ln *> p <* ln'


blk :: Prod r e Token (Token, Loc)
blk = match TokenBlk

blk' :: Prod r e Token (Token, Loc)
blk' = match TokenBlk'


ln :: Prod r e Token (Token, Loc)
ln = match TokenLn

ln' :: Prod r e Token (Token, Loc)
ln' = match TokenLn'


-- -----------------------------------------------------------------------------
-- Symbols

parens :: Prod r e Token a -> Prod r e Token a
parens p = rsvp "(" *> p <* rsvp ")"

parensLoc :: Prod r e Token a -> Prod r e Token (a, Loc)
parensLoc p =
  let ex (_, l1) r (_, l2) = (r, l1 <> l2)
  in ex <$> rsvp "(" <*> p <*> rsvp ")"


braces :: Prod r e Token a -> Prod r e Token a
braces p =
  rsvp "[" *> p <* rsvp "]"

bracesLoc :: Prod r e Token a -> Prod r e Token (a, Loc)
bracesLoc p =
  let ex (_, l1) r (_, l2) = (r, l1 <> l2)
  in ex <$> rsvp "[" <*> p <*> rsvp "]"


curlys :: Prod r e Token a -> Prod r e Token a
curlys p =
  rsvp "{" *> p <* rsvp "}"

curlysLoc :: Prod r e Token a -> Prod r e Token (a, Loc)
curlysLoc p =
  let ex (_, l1) r (_, l2) = (r, l1 <> l2)
  in ex <$> rsvp "{" <*> p <*> rsvp "}"


angled :: Prod r e Token a -> Prod r e Token a
angled p =
  rsvp "<" *> p <* rsvp ">"
           
sep :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep s p =
  (:) <$> p <*> many (s *> p)
  
sep' :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep' s p =
  sep s p <|> pure []

commaSep :: Prod r e Token a -> Prod r e Token [a]
commaSep = sep (rsvp ",")

commaSep' :: Prod r e Token a -> Prod r e Token [a]
commaSep' = sep' (rsvp ",")
  
mono :: Prod r e Token a -> Prod r e Token [a]
mono =
  fmap (:[])
  
prepend :: Prod r e Token a -> Prod r e Token [a] -> Prod r e Token [a]  
prepend =
  liftA2 (:)
