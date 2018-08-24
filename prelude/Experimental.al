-- Objects as syntatic sugar?
Category: id, compose
=> Category = \id c f. f id c
   id      = λe. e (λid comp . id)
   compose = λe. e (λid comp . comp)

-- Still requires dictionary passing style...

Category: id, compose

Function: fn

Function <: Category where
  id = \a. a
  compose = \g f a. g (f a)


foo = \fn.
  case fn of
    (|Function f | Category id _|) -> id f 



-- A list can have a cons or a nil

-- Or maybe we should have ADT-style
List := Nil | Cons item list

-- This way, List is just a pattern that
-- will match on both Nil and Cons
-- It is a short cut for
List :> Nil, Cons
Nil: -- empty objects aren't possible lol
Cons: item list

-- That is, List is a super-structure of nil and cons
-- Nil and cons are substructures of a list.
-- A functor is a concrete super-structure of a list
Functor :> List where
  map = listMap

-- This transforms the substructures of list
> Nil
=> {map}
=> {item, list, map}

-- So we can treat the constructed object
-- as a bag containing all of its goodies

-- List, Nil, and Cons are abstract structures
-- Functor is also an abstract structure
-- But we can make concrete structures, where
-- the values are set.

-- Here is a concrete structure where the field
-- 'bar' is always the value 100.
Foo: bar
  where bar = 100

-- Foo can then be constructed without any fields
> Foo
=> Foo {bar = 100}



-- Now lets get weird with embedding
-- We can embed a Nil or Cons in a list
-- We can further classify lists as a cons

List <: Functor, Applicative, Monad where
  fmap = listMap

-- Objects are very abstract
-- They can also serve as a dictionary
-- Extensible Pattern Views
Functor: fmap
Applicative: pure, ap
Monad: return, bind

-- Extend their definitions!
-- Embed functor in applicative!
Applicative <: Functor
-- Embed applicative in monad!
Monad <: Applicative

Semigroup: append

-- Monoid is an extended semigroup
Monoid <: Semigroup: empty

-- We can just extend some existing object class
List <: Semigroup where
  append = listAppend


-- If we tried this without a semi group
-- the compiler could complain... 
List <: Monoid where
  empty = listEmpty
