module Prelude

id x = x

-- List and friends
data Nil
data Cons head tail

mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

appendList xs ys = foldrList Cons ys xs

foldrList f z Nil = z 
foldrList f z (Cons x xs)
  = f x (foldrList f z xs) 
 
foldlList f z Nil     = z                  
foldlList f z (Cons x xs) = foldlList f (f z x) xs

-- Maybe and friends
Nothing = {}
Just = { value }

mapMaybe f Nothing = Nothing
mapMaybe f (Just v) = Just (f v)

-- Functor
Functor map

listFunctor = Functor mapList
maybeFunctor = Functor mapMaybe

-- Monoid functions
Monoid mempty mappend

listMonoid = Monoid { mempty = Nil, mappend = listAppend }


Vec = { xCoord, yCoord, zCoord }

-- Pattern match on constructor in definitional order
sumVec (Vec x y z) = x + y + z

-- Pattern matching on the constructors record
sumVec v:Vec{xCoord=x, yCoord=y, zCoord=z}
  = x + y + z

-- N.B. record matching does not require all fields

-- You can also just match on constructors
sumVec v:Vec = xCoord v + v.yCoord + v.zCoord

-- Looks like a type, but is actually shorthand for
sumVec v:Vec{} = ...


-- Declared this way, this match will override
-- every other possible input.
-- This is acceptable when a function is only
-- known to work on a single datatype.
sumVec v = xCoord vec + yCoord vec + vec.zCoord

moveUp   v:Vec n = v { yCoord = v.yCoord + n }
moveDown v:Vec n = v { yCoord = v.yCoord - n }