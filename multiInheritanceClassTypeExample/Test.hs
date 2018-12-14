module Test where
import Foo
import Bar

class (Foo a, Bar a) => ABC a where
    haha :: a -> String

test :: ABC abc => abc -> String
test abc = someFunc abc
