# First steps with haskell <a name="First_steps_with_haskell"/>
This project acts as a brain dump in my journey of learning Haskell.
It consists of:
- Installation & basic usage instructions (this README)
- chapters in which i try to explain concepts in my own words (this README)
- codewars challenges (subfolder codewars)
- Haskell Programming From First Principles exercises (subfolder hpffp)
- Random exercises / ideas (*.hs in root folder)


Although i am still far off knowing Haskell fully well, in my opinion it is not a difficult language to apply. It is not even difficult to learn. However it is difficult to teach.
As it is a language with the goal to unify FP principles into one language and give cutting edge research a common ground, many concepts are explained in research papers.
The language & terminology used is many times not beginner friendly.
As Haskell is a pure (here meaning no side effects) language, you need to learn about many concepts first to be able to understand even a very simple program.


There are many attempts to change that. Some of my recommendations (i am in no way affiliated with any of them):
- http://haskellbook.com/ (Haskell Programming From First Principles)
- https://en.wikibooks.org/wiki/Haskell#Beginner's_Track
- https://hoogle.haskell.org/
- https://wiki.haskell.org/Typeclassopedia (great overview over the most important type classes)
- https://fsharpforfunandprofit.com/posts/elevated-world/ (not haskell but still applicable for learning the concepts behind)


If you are looking to learn about / understand something in particular, try a keyword search in this README.


1. [First steps with haskell](#First_steps_with_haskell)
    1. [Installation & Usage](#Installation_&_Usage)
        1. [Installation](#Installation)
        2. [Startup](#Startup)
        3. [Usage](#Usage)
        4. [Configurations](#Configurations)
        5. [IDE](#IDE)
    2. [Concept explanations](#Concept_explanations)
        1. [Type system](#Type_system)
            1. [Static vs dynamic typed](#Static_vs_dynamic_typed)
            2. [Strongly vs weak typed](#Strongly_vs_weak_typed)
            3. [Polymorphism](#Polymorphism)
                1. [Ad hoc / constrained polymorphism](#Ad_hoc_/_constrained_polymorphism)
                2. [Parametric polymorphism](#Parametric_polymorphism)
            4. [Kinds](#Kinds)
            5. [Type, type class, data constructor, type constructor](#Type,_type_class,_data_constructor,_type_constructor)
            6. [Type class](#Type_class)
                1. [Custom type class](#Custom_type_class)
            7. [Newtype vs data vs type](#Newtype_vs_data_vs_type)
        2. [Currying](#Currying)
            1. [Why are function types that strange? Why not just split it into params and result?](#Why_are_function_types_that_strange?_Why_not_just_split_it_into_params_and_result?)
        3. [Pattern matching & guards](#Pattern_matching_&_guards)
        4. [Performance](#Performance)
        5. [Syntactic sugar](#Syntactic_sugar)
        6. [Monoid](#Monoid)
        7. [Functors](#Functors)
            1. [Bifunctors](#Bifunctors)
        8. [Applicative](#Applicative)
            1. [Example for usage in validation](#Example_for_usage_in_validation)
        9. [Monad](#Monad)
            1. [Basic definition](#Basic_definition)
            2. [Explained as container](#Explained_as_container)
            3. [Explained as composition enabler](#Explained_as_composition_enabler)
            4. [Explained in terms of Monoid](#Explained_in_terms_of_Monoid)
    10. [FAQ](#FAQ)
    3. [Contributing](#Contributing)
    4. [License](#License)


## Installation & Usage <a name="Installation_&_Usage"/>

### Installation
see https://docs.haskellstack.org
Install stack, a tool for building (using ghc) and dependency management.

If you just need to build / run single files you may be better of starting with plain ghc:
https://www.haskell.org/ghc/download.html
or get it with your favourite package manager.


### Startup
to startup an interactive haskell interpreter:
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

### Usage
once the module is loaded you may call methods like this:
``` haskell
areStringEq "hello" ('h':'e':'l':'l':'o':[])
```

Examples in this document work best by putting them into a .hs file and loading it with ghci.

### Configurations
Both stack and ghci can be configured.
- ~/.stack/config.yaml
- ~/.ghc/ghci.conf

Feel free to look at my personal configurations in my dotfiles repo: https://github.com/wtjerry/dotfiles

### IDE
In my opinion Haskell is definitely lacking in IDE quality compared to languages like `Java` with IntelliJ or `c#` with VS & ReSharper.

For simple tasks i use vi with the YouCompleteMe plugin. (see my .vimrc at https://github.com/wtjerry/dotfiles)

For projects bigger than one file or more difficult tasks i currently use IntelliJ together with IntelliJ-Haskell (https://plugins.jetbrains.com/plugin/8258-intellij-haskell)

## Concept explanations <a name="Concept_explanations"/>

### Type system <a name="Type_system"/>

Haskell is strongly static typed. Compared to python which would be dynamic weak typed.
see http://learnyouahaskell.com/types-and-typeclasses

functions lower case
Data constructors upper case

#### Static vs dynamic typed <a name="Static_vs_dynamic_typed"/>
is about when type information is acquired (Either at compile time or at runtime)

#### Strongly vs weak typed <a name="Strongly_vs_weak_typed"/>
is about how strictly types are distinguished
Once a "variable" is bound to a value of Type A it cannot be used as if it were of Type B without explicitly converting it.
The following wont work in a strongly typed language:
```
a = "10"
b = a / 2
```

https://stackoverflow.com/a/2351203

#### Polymorphism
There are generally 3 types of polymorphism
- Subtyping
- Ad hoc / constrained polymorphism
- parametric polymorphism

Haskell uses the later two. Subtyping doesn't exist in haskell as there is no concept or hierarchy of types like there is in languages like c#.

##### Ad hoc / constrained polymorphism <a name="Ad_hoc_/_constrained_polymorphism"/>
A function / operator can be overloaded, i.e. change the semantic, depending on what type(s) it is applied to.
In haskell this is usually done with type classes.
In the following example we see the function `length` defined as taking an argument of type `Foldable a` and returning a value of type Int. We may also say `a` is constrained by `Foldable`.

``` haskell
length :: Foldable t => t a -> Int
```

`Foldable` is a type class. `[]` and `Maybe` habe an instance for that typeclass. That's why we can use length with `[]` and also with `Maybe`:

``` haskell
length ['a', 'b', 'c']
-- 3
length $ Just 9
-- 1
length Nothing
-- 0
```

##### Parametric polymorphism <a name="Parametric_polymorphism"/>
Refers to a function or DataType, although they kind of are also functions, with an unconstrained type variable.
In the example of the id function

``` haskell
id :: a -> a
```

we see that a can stand for any type. To be more specific it can stand for any proper type, i.e. of Kind `Type`, or in older examples `*`.
The same goes for the a, b and c in the following examples:

``` haskell
flip :: (a -> b -> c) -> b -> a -> c
Maybe a
```

#### Kinds
Haskell has 3 levels of 'things':
1. Terms / Expressions
2. Types
3. Kinds

(The language is actually in the process of merging Types and Kinds into one level but lets forget that for a moment)

Terms exist at runtime. A few examples of terms are:

``` haskell
42
"Hello World"
[1,2,3]
1 + 2 + 9001
Just "works"
(++) " World"
length
```

Terms inhabit Types, which don't exist at runtime. The previous' examples types are as following:

``` haskell
Integer   -- actually any type constrained by Num
String    -- actually [Char] as string is just a type alias
[Integer] -- again not just Integer but could be any type constrained by Num
Integer   -- same as with 42
Maybe String
String -> String
Foldable t => t a -> Int
```

All but the last 2 look similar. When fully reduced, those terms are just constants.
But the last 2 are are still functions. They still need an argument to be able to be fully reduced to a value.

Now we have seen what a type is. But how can we abstract even further?
Consider the following 3 Data types:

``` haskell
Integer
Maybe a
Either a b
```

They all kind of look different. Lets look at their kinds: (:k in ghci)
```
Integer :: *
Maybe   :: * -> *
Either  :: * -> * -> *
```

The symbol `*` is a synonym to Type here.
The `::` part here doesn't indicate the type as with functions, but the kind.
`Integer` has no free type variable, it is a fully applied type, and therefore has kind `*`.
`Maybe` still needs one type variable to be applied. `Maybe String` for example would be of kind `*`


#### Type, type class, data constructor, type constructor <a name="Type,_type_class,_data_constructor,_type_constructor"/>

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


#### Type class <a name="Type_class"/>

A type class defines a set of functions that is will be available to all instances of that class.
a type can be made an instance of such a class by either using the deriving part when creating the type, or using the instance of construct.
Classes sometimes provide default implementations for the defined functions. They can be overridden by using the instance of construct.

To see the instances of a type class you may use :info YourTypeClass
The Num class defines: (+), (*), abs, signum, fromInteger, (negate | (-))
Therefore whenever a Num is required by a method, an Integer (or others) may be used, similar to an interface in Java
difference to interface:
``` haskell
addNum :: Num c => c -> c -> c
addNum x y = x + y
```

in this example addNum takes a type of class Num and returns a the same type.
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

##### Custom type class <a name="Custom_type_class"/>

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


#### Newtype vs data vs type <a name="Newtype_vs_data_vs_type"/>

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

Internally when a newtype is used GHC doesn't need to use indirection but can treat the Type Identity and the contained value a as the same.
https://wiki.haskell.org/Newtype


### Currying

``` haskell
map (+ 1) [1, 2, 3]
```

'+ 1' is a partially applied function.
(+) is a function of type:
(+) :: Num a => a -> a -> a
it takes 2 parameters of type class Num and returns one result of type class Num.
In the example above one 1 parameter was applied to the (+) function. The result of that is a new function with the definition Num a => a -> a
Once another parameter is applied a type class Num is returned. This happens when map applies each element to that function.

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

#### Why are function types that strange? Why not just split it into params and result? <a name="Why_are_function_types_that_strange?_Why_not_just_split_it_into_params_and_result?"/>
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


### Pattern matching & guards <a name="Pattern_matching_&_guards"/>

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


### Performance

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


### Syntactic sugar <a name="Syntactic_sugar"/>

[a..b] desugars into enumFromTo a b

enumFromTo :: Enum a => a -> a -> [a]


### Monoid

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


### Functors

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

The kind of Functor is `* -> *`. Therefore `Maybe` matches perfectly. However `Either` is of kind `* -> * -> *` and can therefore not have an instance of Functor like this.
The Functor instance of Either is allows you to only map a function over the `Right` values:

``` haskell
data Either a b = Left a | Right b
instance Functor (Either a) where
    fmap _ (Left a) = Left a
    fmap f (Right b) = Right $ f b
```

#### Bifunctor

We saw the Functor instance for `Either a` but what if we want to map over `Left` and `Right` values?
Or more abstract what if we want to create a Functor for Types of kind `* -> * -> *`?
That's when we use the Bifunctor type class:

``` haskell
class Bifunctor p where
    bimap :: (a -> c) -> (b -> d) -> p a b -> p c d
```

Notice `p` here is of kind `* -> * -> *`.
The `Either` instance looks like this:

``` haskell
instance Bifunctor Either where
    bimap f _ (Left a) = Left $ f a
    bimap _ g (Right b) = Right $ g b
```


### Applicative

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
pure f <*> a <*> b -- is equivalent to the fmap version
```
In this example we could not have done something like `fmap b (fmap f a)` as `fmap f a` results in a `Maybe (a -> a)` but fmap can only map functions of type `(a -> a)`.

#### Example for usage in validation <a name="Example_for_usage_in_validation"/>

Credits: Chapter 12 of http://haskellbook.com/
Given the following types:

``` haskell
data Person = MkP String Integer deriving (Show)
data PersonInvalid = NameInvalid | AgeToLow deriving (Show)
nameOkay :: String -> Either [PersonInvalid] String
ageOkay :: Integer -> Either [PersonInvalid] Integer
```

it would be nice to create a function that safely constructs a Person or returns ALL validation errors that occurred.
Exactly that is possible by lifting the `MkP` function into Applicative space with `liftA2`.

``` haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

We can transform the function

``` haskell
MkP :: String -> Integer -> Person
```

into a function with signature

``` haskell
   Either [PersonInvalid] String
-> Either [PersonInvalid] Integer
-> Either [PersonInvalid] Person
```

We can see that the type variable `f` in the `liftA2` signature takes on `Either [PersonInvalid]`

Applying this new knowledge we can finally create that validation function:

``` haskell
mkPerson :: String -> Integer -> Either [PersonInvalid] Person
mkPerson name age = liftA2 MkP (nameOkay name) (ageOkay age)
```

### Monad

#### Basic definition <a name="Basic_definition"/>
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

#### Explained as container <a name="Explained_as_container"/>
A monad is a type constructor and a container that supports basic functions to **wrap and unwrap functions as values**.

https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative

#### Explained as composition enabler <a name="Explained_as_composition_enabler"/>
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
The following series of posts has used railway switches as a metaphor, which in my helps the understanding for Monads like Maybe and Either but not so much for Monads like List or State: https://fsharpforfunandprofit.com/posts/elevated-world-3/

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

#### Explained in terms of Monoid <a name="Explained_in_terms_of_Monoid"/>
Link: https://www.stackoverflow.com/a/7829607 :
"a monad is a structure that defines a way to combine (the results of) functions,
analogously to how a monoid is a structure that defines a way to combine objects"

While (+) defines the combination (addition) of numbers and (++) defines the combination (concatenation) of lists there is something similar for Monads. In Haskell this function is called join (flatten in F#)
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


### FAQ
- what does bottom mean?

bottom also written as `_|_ ` is a member of every type. It represents an infinite / failed computation.
What does "is member of" mean? If we have a type Gender
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


- what is a value constructor?

synonym for data constructor


- what does inhabited / uninhabited type mean?

A type is inhabited if there is at least one term of that type.
In contrary it is uninhabited if there is no term of that type / it cannot be constructed.
Data.Void is one example of an uninhabited type.


- What is the difference between Monad, Monoid and monadic?

While the terms Monad and monadic are related, Monoid is a different concept.
Monad is a type class as seen above.
Monadic code is code that uses Monad functions (eg. >>, >>= and return).
Monoid is just another type class that is defined as having a identity element (mempty), a function mappend and a function mconcat. In Data.Monoid <> is defined as an operator synonym for mappend.
https://en.wikibooks.org/wiki/Haskell/Monoids


## Contributing
Feel free to create a pull request.


## License
This project is released under the GPLv3 license.

