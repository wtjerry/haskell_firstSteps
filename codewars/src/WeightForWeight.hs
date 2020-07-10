module Codewars.G964.WeightSort where

import Data.Char
import Data.List
import Data.Ord

-- my original version:
-- orderWeight :: [Char] -> [Char]
-- orderWeight = unwords . map snd . sort . map (\x -> (sumOfDigits x, x)) . words
--     where sumOfDigits = sum . map digitToInt

-- after getting inspired by other solutions
orderWeight = unwords . sortBy (comparing weight) . words
  where weight = sum . map digitToInt
