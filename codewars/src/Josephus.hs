module Josephus where

-- import Debug.Trace (trace)

josephus [] _ = []
josephus (x : []) _ = [x]
-- josephus xs k | trace ("--- " ++ show xs ++ " - " ++ show k ++ " ---") False = undefined
josephus xs k = (xs !! index) : (josephus (splitAtAndRemoveAndReorder xs index) k)
  where
    index = if k > len then ((k -1) `mod` len) else (k -1)
    len = length xs

splitAtAndRemoveAndReorder xs k = (drop (k + 1) xs) ++ (take k xs)

