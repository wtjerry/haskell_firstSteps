module Collatz where

collatz 1 = [1]
collatz x = if (even x) then x:(collatz (div x 2)) else x:(collatz (3*x+1))

numOfLongChains longness start = length (filter isLong (map collatz [1..start]))
    where isLong xs = length xs > longness
