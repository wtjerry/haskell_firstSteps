module BreakCamelCase where

import Data.Char

solution :: String -> String
solution = (dropWhile isSpace) . camelCase

camelCase :: String -> String
camelCase [] = []
camelCase (x : xs)
  | isLower x = x : (camelCase xs)
  | otherwise = ' ' : x : (camelCase xs)
