module WeirdIndex where

import Data.Char

solve :: String -> String
solve = unwords . map toWeirdCase' . words

toWeirdCase' :: String -> String
toWeirdCase' s = map f $ zip (map toLower s) [0 ..]
  where
    f (c, i) = if (i `mod` 2 == 0) then toUpper c else c
