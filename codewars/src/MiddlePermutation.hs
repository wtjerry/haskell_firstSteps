module Lib where

import Data.List

middlePermutation :: String -> String
middlePermutation myString =
  if odd (length s)
    then middleO1 : middleO2 : reverse (filter ((&&) <$> (/= middleO1) <*> (/= middleO2)) s)
    else middleE : reverse (filter (/= middleE) s)
  where
    middleO1 = s !! middle
    middleO2 = s !! (middle - 1)
    middleE = s !! (middle - 1)
    middle = div (length s) 2
    s = sort myString