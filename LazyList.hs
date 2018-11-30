module LazyList where

everyXth increment start = [start, start+increment ..]
every3rd = everyXth 3
every5th = everyXth 5
every8th start = zipWith (+) (every3rd start) (every5th start)

isDivisible a b = (mod a b) == 0
isPrime x = not (or (map (isDivisible x) [2..x-1]))
primes = [x | x <- [3..], isPrime x]

-- bs String breakAtChars currentChars
-- bs :: [Char] -> Integer -> Integer -> [Char]
-- bs [] _ _ = []
-- bs (' ':xs) max current = if current > max 
--     then '\n':(bs xs max 0)
--     else ' ':(bs xs max (current+1))
-- bs (x:xs) max current = x:(bs xs max (current+1))

bs :: [Char] -> Integer -> Integer -> Integer -> [Char]
bs [] _ _ _ = []
bs (' ':xs) max current line = if current > max 
    then '\n':(show line) ++ (bs xs max 0 (line+1))
    else ' ':(bs xs max (current+1) line)
bs (x:xs) max current line = x:(bs xs max (current+1) line)

breakString s max = bs ('1':s) max 0 2
