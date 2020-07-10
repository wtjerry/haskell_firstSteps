module FindTheOddInt where

import Data.List

findOdd :: [Int] -> Int
findOdd = head . head . filter (odd . length) . group . sort
