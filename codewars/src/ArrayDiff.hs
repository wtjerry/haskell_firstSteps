module ArrayDiff where

difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference a [] = a
difference a (b:bs) = difference (filter (/= b) a) bs
