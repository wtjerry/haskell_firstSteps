module PriceDraw where

import Data.Char
import Data.List

rank :: [Char] -> [Int] -> Int -> [Char]
rank [] _ _ = "No participants"
rank st we n
  | n > length names = "Not enough participants"
  | otherwise = snd $ ranked names we !! (n -1)
  where
    names = wordsBy ',' st

ranked names weights = sortBy numberThenName $ zip winningNumbers names
  where
    winningNumbers = zipWith winningNumber names weights
    numberThenName (wn1, n1) (wn2, n2) = mappend (compare wn2 wn1) (compare n1 n2)

winningNumber name weight = weight * (length name + (sum . map ascii) name)
  where
    ascii c = (ord . toUpper) c -64

wordsBy :: Char -> String -> [String]
wordsBy delim s = (words . map delimToSpace) s
  where
    delimToSpace c = if c == delim then ' ' else c
