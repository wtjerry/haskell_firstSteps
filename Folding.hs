module Folding where

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

functionComposition = foldr (.) id [(+2), (*7)]

elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = foldl (\x y -> x || e == y) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x):acc) [] xs

slowMap' :: (a -> b) -> [a] -> [b]
slowMap' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

reverseMap' :: (a -> b) -> [a] -> [b]
reverseMap' f xs = foldl (\acc x -> (f x):acc) [] xs
