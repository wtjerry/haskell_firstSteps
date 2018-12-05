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

### usage
once the module is loaded you may call methods like this:
``` haskell
areStringEq "hello" ('h':'e':'l':'l':'o':[])
```

## type system

Haskell is strongly static typed. Compared to python which would be dynamic weak typed.
see http://learnyouahaskell.com/types-and-typeclasses

### static vs dynamic typed:

Once a "variable" is bound to a value, it cannot be bound to a value of a differnt type.
The following wont work in a static typed language:
```
x = "hello world"
x = 5
```

### strongly vs weak typed:

Once a "variable" is bound to a value of Type A it cannot be used as if it were Type B without explicitly converting it.
The following wont work in a strongly typed language:
```
a = "10"
b = a / 2
```


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


## class type

class: a class defines and implements a set of functions that is will be avaialbe to all types of that class.
the Num class defines: (+), (*), abs, signum, fromInteger, (negate | (-))
therefore whenver a Num is required by a method an Integer (or others) may be used, similar to an interface in Java
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

i1 = toInteger 1
i2 = toInteger 2
showDouble (addNum i1 i2)
```

here i1 and i2 are of type Integer and showDouble only takes a Doulbe. Both Integer and Double would be of class Num. But when calling the function showDouble with the output of addNum while passing 2 Integers we receive an Exception.

functions lower case
Data constructors upper case


## currying

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

Prelude> (\x -> \y -> x * y) 2 3
6
Prelude> :t (\x -> \y -> x * y) 2
(\x -> \y -> x * y) 2 :: Num a => a -> a

### Why are function types that strange? Why not just split it into params and result?
In haskell every function is curried.
(+) :: Num a => a -> a -> a
the order of evaluating goes as follows:
(\x -> \y -> x + y) 1 2
(\x -> 1 + x) 2
(1 + 2)
3

and the types will be:
Num a => a -> a -> a
Num a => a -> a
Num a => a
Num a => a


## monad
A monad is a type constructor and a container that supports basic functions to **wrap and unwrap functions as values**.

https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative


## pattern matching & guards

``` haskell
length [] = 0
length (x:xs) = 1 + length xs
```
or
``` haskell
data Gender = Female | Male
data Human = Human Float Gender -- Float is the age
isRetired (Human age Female) = age >= 64
isRetired (Human age Male) = age >= 65
```
is the same as
``` haskell
data Gender = Female | Male
data Human = Human Float Gender -- Float is the age
isRetired h = case h of (Human age Female) -> age >= 64
                        (Human age Male) -> age >= 65
```

vs.

``` haskell
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
```
or
``` haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list." 
                                               xs -> "a longer list."
```


## performance

As with every language the way you solve a problem affects the performance.
Even if haskell is a functional language and you dont really tell it how to solve a problem but more what you expect and define some edge cases, you will see differences.
Let's look at the following example:

ghci> :set +s

ghci> last [x | x <- [1..10000000], mod x 3829 == 0]
9997519
(5.49 secs, 2,560,263,768 bytes)

ghci> head (filter (\x -> mod x 3829 == 0) [10000000,9999999..0])
9997519
(0.01 secs, 730,160 bytes)

There is a quite big difference in whether we build up a list and take the last or just take the first. Both in execution time 5.5 sec vs basically instantaneous and memory 2.5 GB vs 730 Kb.
