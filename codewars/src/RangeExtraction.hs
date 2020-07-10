module RangeExtraction where

import Data.List

data Range
  = Single Int
  | Range Int Int

instance Show Range where
  show (Single x) = show x
  show (Range a b) = show a ++ "-" ++ show b

type B = ([Range], Maybe Int, Maybe Int)

solution :: [Int] -> String
solution = intercalate "," . map show . reverse . toRanges

toRanges :: [Int] -> [Range]
toRanges = extract . foldl scanner ([], Nothing, Nothing)

scanner :: B -> Int -> B
scanner (r, Nothing, _) x = (r, Just x, Just x)
scanner (ranges, Just start, Just prev) x
  | x > (prev + 1) && start == prev = (Single start : ranges, Just x, Just x)
  | x > (prev + 1) && start + 1 == prev = (Single prev : Single start : ranges, Just x, Just x)
  | x > (prev + 1) = (Range start prev : ranges, Just x, Just x)
  | otherwise = (ranges, Just start, Just x)

extract :: B -> [Range]
extract (ranges, Nothing, Nothing) = ranges
extract (ranges, Just start, Just prev)
  | start == prev = Single start : ranges
  | start + 1 == prev = Single prev : Single start : ranges
  | otherwise = Range start prev : ranges
