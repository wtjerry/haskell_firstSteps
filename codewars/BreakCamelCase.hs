module Codewars.Kata.BreakCamelCase where

import Data.Char

solution :: String -> String
solution = (dropWhile isSpace) . camelCase

camelCase [] = []
camelCase (x:xs)
    | isLower x = x:(camelCase xs)
    | otherwise = ' ':x:(camelCase xs)
