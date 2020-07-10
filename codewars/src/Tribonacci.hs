module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n $ trib a b c

add3 a b c = a + b + c

trib a b c = a : b : c : zipWith3 add3 shifted0 shifted1 shifted2
  where
    shifted0 = trib a b c
    shifted1 = tail shifted0
    shifted2 = tail shifted1
