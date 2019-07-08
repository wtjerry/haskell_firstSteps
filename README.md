# First steps in haskell
The following should act as a walkthrough from installing haskell, over loading your first module, to running your first small program.

## Installation
apt install haskell-platform

## startup
to startup up a interactive haskell interpreter:
``` sh
ghci
``` 
afterwards to load, reload modules:
``` sh
:l FirstSteps.hs
:r
```
to find out about a method / type:
``` sh
:t map 
```

## usage
once the module is loaded you may call methods like this:
``` haskell
areStringEq "hello" ('h':'e':'l':'l':'o':[])
```

Examples in this document work best by putting them into a hs file and loading it with ghci.
They may however also work if 'let' is used to bind functions.

# type system

Haskell is strongly static typed. Compared to python which would be dynamic weak typed.
see http://learnyouahaskell.com/types-and-typeclasses

functions lower case
Data constructors upper case

## static vs dynamic typed:
is about when type information is acquired (Either at compile time or at runtime)

## strongly vs weak typed:
is about how strictly types are distinguished
Once a "variable" is bound to a value of Type A it cannot be used as if it were of Type B without explicitly converting it.
The following wont work in a strongly typed language:
```
a = "10"
b = a / 2
```

https://stackoverflow.com/a/2351203

## type, type class, data constructor, type constructor

type:
- Bool
- String
- Float
- Integer
- ..


type class (image: https://en.wikibooks.org/wiki/Haskell/Classes_and_types):
- Num
- Eq
- Show
- Enum
- ..


data constructor (https://stackoverflow.com/questions/18204308/haskell-type-vs-data-constructor):
``` haskell
data Gender = Male | Female
```
Gender would be the type.
Male and Female are both nullary data constructors.
Male :: Gender
Female :: Gender

``` haskell
data Color = RGB Int Int Int
```
Color is again a type.
But RGB is a ternary data constructor.
RGB :: Int -> Int -> Int -> Color


type constructor:
``` haskell
data Maybe a = Nothing | Just a
```
Nothing is again a nullary data constructor.
Just is again a unary data constructor.
But Maybe is not a type but a type constructor.
So something cannot be of type Maybe. It can however be of type Maybe String.


## type class

A type class defines a set of functions that is will be avaialbe to all instances of that class.
a type can be made an instance of such a class by either using the deriving part when creating the type, or using the instance of construct.
Classes sometimes provide default implementations for the defined functions. They can be overriden by using the instance of construct.

To see the instances of a type class you may use :info YourTypeClass
The Num class defines: (+), (*), abs, signum, fromInteger, (negate | (-))
Therefore whenver a Num is required by a method, an Integer (or others) may be used, similar to an interface in Java
difference to interface:
``` haskell
addNum :: Num c => c -> c -> c
addNum x y = x + y
```

in this exmample addNum takes a type of class Num and returns a the same type.
If we would pass an Integer to addNum we would be guaranteed to receive an Integer and not just any Type of class Num.
To proof this:

``` haskell
showDouble :: Double -> String
showDouble d = show d

i1 :: Int
i1 = 1
i2 :: Int
i2 = 2
showDouble (addNum i1 i2)
```

here i1 and i2 are of type Integer and showDouble only takes a Double. Both Integer and Double would be of class Num. But when calling the function showDouble with the output of addNum while passing 2 Integers we receive an Exception.

### custom type class

``` haskell
data Color = Green | Blue | Red | Yellow deriving (Show)

class Colorable a where
    colorOf :: a -> Color

instance Colorable Integer where
    colorOf 42 = Green
    colorOf 0 = Yellow
    colorOf i | i < 0 = Red
        colorOf _ = Blue
```


## newtype vs data vs type

While newtype and data are able to create new types, type only creates a synonym.
``` haskell
type String = [Char]
```

newtype can only take one data constructor with one field.
``` haskell
newtype Identity a = Identity a
```
but not
``` haskell
newtype Pair a b = Pair a b
```

Internally when a newtype is used GHC doesnt need to use indirection but can treat the Type Identity and the contained value a as the same.
https://wiki.haskell.org/Newtype


# currying

``` haskell
map (+ 1) [1, 2, 3]
```

'+ 1' is a partially applied function.
(+) is a function of type:
(+) :: Num a => a -> a -> a
it takes 2 parameters of type class Num and returns one result of type class Num.
In the example above one 1 paramter was applied to the + function. The result of that is a new function with the definition Num a => a -> a
Once another paramater is applied a type class Num is returned. This happends when map applies each element to that function.

Another example (use :t to see function definition)
``` haskell
add3Numbers1 a b c = a + b + c
add3Numbers2 = add3Numbers 1
add3Numbers3 = add3Numbers2 2
```

``` haskell
prod x y = x * y
double = prod 2
tripple = prod 3
```

```
Prelude> (\x -> \y -> x * y) 2 3
6
Prelude> :t (\x -> \y -> x * y) 2
(\x -> \y -> x * y) 2 :: Num a => a -> a
```

## Why are function types that strange? Why not just split it into params and result?
In haskell every function is curried.
```
(+) :: Num a => a -> a -> a
the order of evaluating goes as follows:
(\x -> \y -> x + y) 1 2
(\x -> 1 + x) 2
(1 + 2)
3
```

and the types will be:
```
Num a => a -> a -> a
Num a => a -> a
Num a => a
Num a => a
```


# pattern matching & guards

pattern matching on function level:
``` haskell
length [] = 0
length (x:xs) = 1 + length xs
```
or:
``` haskell
data Gender = Female | Male
data Human = Human Float Gender -- Float is the age
isRetired (Human age Female) = age >= 64
isRetired (Human age Male) = age >= 65
```

pattern matching with case expression:
``` haskell
data Gender = Female | Male
data Human = Human Float Gender -- Float is the age
isRetired h = case h of (Human age Female) -> age >= 64
                        (Human age Male) -> age >= 65
```

guards on function level:
``` haskell
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
```

guards with case expression:
``` haskell
describeMaybeInt :: Maybe Int -> String
describeMaybeInt m = "The int " ++ case m of
    Just i | i > 0 -> "is positive."
           | i == 0 -> "is zero."
           | otherwise -> "is negative."
    Nothing -> "doesn't exist."
```


# performance

As with every language the way you solve a problem affects the performance.
Even if haskell is a functional language and you dont really tell it how to solve a problem but more what you expect and define some edge cases, you will see differences.
Let's look at the following example:

```
ghci> :set +s
ghci> last [x | x <- [1..10000000], mod x 3829 == 0]
9997519
(5.49 secs, 2,560,263,768 bytes)
ghci> head (filter (\x -> mod x 3829 == 0) [10000000,9999999..0])
9997519
(0.01 secs, 730,160 bytes)
```

There is a quite big difference in whether we build up a list and take the last or just take the first. Both in execution time 5.5 sec vs basically instantaneous and memory 2.5 GB vs 730 Kb.


# syntactic sugar

[a..b] desugars into enumFromTo a b

enumFromTo :: Enum a => a -> a -> [a]


# Monoid

``` haskell
Monoid m
mempty x = x :: a -> a       -- aka id
mappend :: m a -> m a -> m a -- aka <>
```

They have to follow 2 laws:
Associativity:
``` haskell
(a . b) . c = a . (b . c)
```
Identity:
``` haskell
f . mempty = mempty . f = f
```

To form a Monoid there has to be a set of values, an associative operation and an identity element.
The type class Num, with the operation addition and the identity element 0 fulfills these requirement.
The type class Ordering, with the identity element `EQ` forms a Monoid aswell. That way multiple orderings can be combined like so:
``` haskell
import Data.List
import Data.Ord

a = [(1, "dd"), (3, "zz"), (1, "aa")]
ascendingNumThenDescendingString (n1, s1) (n2, s2) = mappend (compare n1 n2) (compare (Down s1) (Down s2))

sortBy ascendingNumThenDescendingString a  -- [(1,"dd"),(1,"aa"),(3,"zz")]
```


# Functors

A functor is just another type class like Num or our example Colorable.
``` haskell
Functor f
fmap :: (a -> b) -> f a -> f b  -- aka <$>
```
That means each type that has an instance of Functor can be mapped over with a function.
`[]` is such a type (`[]` is actually just a type constructor but lets let that slide for now). 

In the case of `[]`, `fmap` is just `map`.

Maybe is another instance of Functor:
``` haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```
When applying a function f to Nothing the result will be Nothing.
When applying a function f to a (Just x) it will unpack the (Just x) and apply the function to x and repack it again.

<$> is the infix version of fmap


# Applicative

``` haskell
Applicative f
pure :: a -> f a
<*> :: f (a -> b) -> f a -> f b  -- aka apply
*> :: f a -> f b -> f b
```

``` haskell
fmap (+) [1..10] <*> pure 1
```

``` haskell
fmap (*) (Just 10) <*> pure 3
```

Applicatives can be used for functions with multiple arguments while having impure (like Maybe a) values as parameters:
``` haskell
f = (+)
a = Just 1
b = Just 4

fmap f a <*> b     -- Just 5
pure f <*> a <*> b -- is euqivalent to the fmap version
```
In this example we could not have done someting like `fmap b (fmap f a)` as `fmap f a` results in a `Maybe (a -> a)` but fmap can only map functions of type `(a -> a)`.


# monad

## basic definition
``` haskell
Monad m
return :: a -> m a
>> :: m a -> m b -> m b         -- is called then
>>= :: m a -> (a -> m b) -> m b -- is called bind (aka flatMap in F#)
```

In contrast to apply (<*>) from the Applicative Functor, bind is NOT structure preserving as the following example shows:
``` haskell
a = [1, 3]

f :: (Num a) => a -> a
f = (+) 1

g :: a -> [a]
g = replicate 3

fmap f a     -- [2,4]
pure f <*> a -- [2,4]
pure g <*> a -- [[1,1,1],[3,3,3]
a >>= g      -- [1,1,1,3,3,3]
```

## explained as container
A monad is a type constructor and a container that supports basic functions to **wrap and unwrap functions as values**.

https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative

## explained as composition enabler
A normal function uses a regular arrow like so:
``` haskell
f :: A -> B
g :: B -> C
```
and can easily be composed:
``` haskell
h :: A -> C
f . g = h
```

The following are Kleisli arrows:
```
f' :: A -> D B
g' :: B -> D C
```
where D might be a List, a Maybe or any other monadic Datatype.
These cannot be composed so easily.
The Monad Design Pattern is there to solve this problem. Each Datatype that has an instance of Monad defines in that instance how those arrows can be composed.
The following series of posts has used railway switches as a metaphor, which in my helps the understanding for Monads like Maybe and Either but not so much for Monds like List or State: https://fsharpforfunandprofit.com/posts/elevated-world-3/

The definition for Maybe looks like the following:
``` haskell
data Maybe a = Nothing | Just a

instance Monad Maybe where
return = Just
a >> b = b
Nothing >>= _ = Nothing
(Just a) >>= f = f a
```

If Maybe hadn't an instance of Monad and any function in a codebase may fail and therefore return a Maybe a, all other functions would have to be changed to take and return a Maybe a.
Also each one would have to pattern match for Nothing on the input.
With the instance of Monad, the other functions can stay and just be used with bind.

## explained in terms of Monoid
Link: https://www.stackoverflow.com/a/7829607 :
"a monad is a structure that defines a way to combine (the results of) functions,
analogously to how a monoid is a structure that defines a way to combine objects"

While (+) defines the combination (addition) of numbers and (++) defines the combination (concatination) of lists there is something similar for Monads. In Haskell this function is called join (flatten in F#)
``` haskell
join :: (Monad m) => m (m a) -> m a
```

If we want to compose 2 functions of the form 
`a -> m a`
bind could be explained like so:

1. compute the results of the first function
2. apply the second function to each result
3. combine it

for the List Monad the output of each step could look like the following:

1. `[1, 2, 3]`
2. `[[1, 1], [2, 2], [3, 3]]`
3. `[1, 1, 2, 2, 3, 3]`


# etc
- what does bottom mean?

bottom also written as `_|_ ` is a member of every type. It represents an infinite / failed computation.
What does "is memer of" mean? If we have a type Gender
```haskell
data Gender = Female | Male
```
which has 2 members (aka data constructor) it implicitly has bottom as a 3rd member.


- What is the Unit type aka ()?

```
ghci> :t ()
ghci> () :: ()
```
That means the value () is of type ().
The unit type just means there is only one value / member to this type. It is quite boring really.
It is therefore used whenever some function doesnt return anything interesting but is just used for its effect.
One example are the functions that return an IO Monad of type IO ().
```haskell
ghci> :t putStr
ghci> putStr :: String -> IO ()
```
Because putStr just writes its input to stdout it doesnt return anything useful.


- what is a value contructor?

synonym for data constructor


- what does inhabited / uninhabited type mean?

A type is inhabited if there is at least one term of that type.
In contratry it is uninhabited if there is no term of that type / it cannot be constructed.
Data.Void is one example of an uninhabited type.


- What is the difference between Monad, Monoid and monadic?

While the terms Monad and monadic are related, Monoid is a different concept.
Monad is a type class as seen above.
Monadic code is code that uses Monad functions (eg. >>, >>= and return).
Monoid is just another type class that is defined as having a identity element (mempty), a function mappend and a function mconcat. In Data.Monoid <> is defined as an operator synonym for mappend.
https://en.wikibooks.org/wiki/Haskell/Monoids

