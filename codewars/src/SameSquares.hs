module SameSquares where

import Data.List (sort)

comp4 :: (Num a, Ord a) => [a] -> [a] -> Bool
comp4 [] [] = True
comp4 as bs = (sort bs) == (sort $ map (^ 2) as)
