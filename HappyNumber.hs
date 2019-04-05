module HappyNumber(isHappy, isHappyShort) where

import Data.Char (digitToInt)

splitSquareAndSumSequence start = scanl toTuple (start, []) (tail (gen start))
    where toTuple (i, l) a = (a, i:l)
          gen i = i : (gen $ splitSquareAndSum i)
          splitSquareAndSum i = sum $ map ((^2) . digitToInt) (show i)

isHappy i = 1 == (fst $ head $ dropWhile neitherOneNorRepetition $ splitSquareAndSumSequence i)
    where neitherOneNorRepetition (a, b) = and [a /= 1, notElem a b]

isHappyShort i
    | i == 1 = True
    | i == 4 = False
    | otherwise = isHappyShort $ splitSquareAndSum i
    where splitSquareAndSum i = sum $ map ((^2) . digitToInt) (show i)
