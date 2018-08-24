-- Types as functions
-- Good luck checking these!
int = \i f. f i
bool = \p f. f p
string = \s f. f s
char = \c f. f c
array = \a f. f a

-- In general, we can embed terms
embed = \a f . f a


-- Self application
omega = \x. x x

-- Loop, infinitely self applied
Omega = omega omega

-- Category
category = \id comp. 
  (\c. c id comp)

id      = λe. e (λid comp . id)
compose = λe. e (λid comp . comp)

const   = \a b . a

-- Category for function
idFn = λx. x
composeFn = λf g. λx. g (f x)

fnCategory = category idFn composeFn


-- Isomorphism
isomorphism = \f g iso. iso f g

to   = \e. e true
from = \e. e false

-- Isomorphism for functions

fnIso = \f g. isomorphism f g

-- Adjunction

adjunction = \f u c l r adj. adj f u c l r
adjunctionFunctor = \e. e (\f u c l r. f)

unit         = \e. e (\f u c l r. u)
counit       = \e. e (\f u c l r. c)
leftAdjunct  = \e. e (\f u c l r. l)
rightAdjunct = \e. e (\f u c l r. r)

unitDef         = \a. leftAdjunct (id fnCategory) a
counitDef       = \fa. rightAdjunct (id fnCategory) fa
--leftAdjunctDef  = \f a e. fmap (adjunctionFunctor e) f (unit e a)
--rightAdjunctDef = \f fa e. counit (fmap (adjunctionFunctor e) f fa)

-- Adjuction between Pair and function

--funLeftAdj = \f. id fnCategory (f (id fnCategory x))
--funLeftAdj = \f. id fnCategory (f (id fnCategory x))

--functionAdjunction = adjunction functionFunctor (unitDef functionAdjunction) (counitDef functionAdjunction) funLeftAdj funRightAdj

-- Monoid
monoid = λempty append. pair empty append

empty  = fst
append = snd

-- Functor
functor = λmap. map
map    = λe. e


-- BiFunctor
bifunctor = λbimap. bimap
bimap    = λe. id e


-- Monad
monad  = λbind return. λmonad. monad bind return
bind   = λe. e (λbind return. bind) 
return = λe. e (λbind return. return)

join   = λe x. bind e x id
liftM  = λe f m. bind e m (λx. return e (f x))


-- Boolean logic
true  = λt f. t
false = λt f. f

if = λc. \t f. c t f

and = λx y. λt f. x (y t f) f
or  = λx y. λt f. x t (y t f)
not = λx  . λt f. x f t


-- Pairs, Products
pair  = λx y p. p x y
fst   = λp. p true
snd   = λp. p false

swap  = λp. pair (snd p) (fst p)

bimapPair = λf g p. pair (f (fst p)) (g (snd p))
pairBifunctor = bifunctor bimapPair


-- Either, Sum, Coproduct
left  = λa l r. l a
right = λb l r. r b


-- Maybe
nothing = λ  n j. n
just    = λx n j. j x

maybe = λd f mb. mb d f
isNothing = λmb. mb true (const false)
isJust    = λmb. mb false (const true)
fromJust  = λmb. mb _ (idFn)

-- Maybe functor
mapMaybe = λf mb. mb nothing (\x. just (f x))
maybeFunctor = mapMaybe

-- Monad Evidence
bindMaybe = λmb f. mb nothing f
returnMaybe = just

maybeMonad = pair bindMaybe returnMaybe


-- Lists!
nil  = λc n. n
cons = λh t c n. c h (t c n)
head = λl. l (λh t. h) false
tail = λl c n. l (λh t g. g h (t c)) (λt. n) (λh t. t)

foldr = _

listAppend = \la lb . foldr cons lb la
listMonoid = monoid nil listAppend