module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence s = foldr1 max $ map maxL $ per s
  where
    per (x : []) = [[x]]
    per l@(_ : xs) = l : (per xs)
    per [] = error "should not happen as [] is already checked by top level function maxSequence"
    maxL l = foldr max 0 $ scanl1 (+) l
