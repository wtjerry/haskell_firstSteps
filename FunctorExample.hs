module FunctorExample where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node val a b) = Node (f val) (fmap f a) (fmap f b)


demoTree = Node 42 EmptyTree (singleton 9)
demoTree2 = fmap (*2) demoTree

