module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence s = foldr1 max $ map maxL $ per s
  where
    per (x : []) = [[x]]
    per l@(x : xs) = l : (per xs)
    maxL l = foldr max 0 $ scanl1 (+) l
