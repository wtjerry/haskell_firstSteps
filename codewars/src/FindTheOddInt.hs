module Codewars.Kata.FindOdd where

import Data.List

findOdd :: [Int] -> Int
findOdd = head . head . filter (odd . length)  . group . sort

