module StrinToNum where
import Data.Char

size [] = 0
size (x:xs) = 1 + size xs

charToDigit c
    | isDigit c = read [c]
    | otherwise = 0

s2n :: String -> Int
s2n [] = 0
s2n (x:xs) = (s2n xs) + ((charToDigit x) * (10 ^ (size xs)))

