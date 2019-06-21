module PileOfQubes where

-- 1^3 + 2^3 + 3^3 + .. n^3 = ((n * (n + 1)) `div` 2) ^ 2 = nToMass
-- solve for n => sqrt (2 * sqrt mass) = massToN
-- if mass == (nToMass . massToN) mass while using integerSquareRoot then a solution could be found
findNb :: Integer -> Integer
findNb mass =
  if mass == (nToMass . massToN) mass
    then massToN mass
    else (-1)
  where
    nToMass n = ((n * (n + 1)) `div` 2) ^ 2
    massToN mass = integerSquareRoot (2 * integerSquareRoot mass)
    integerSquareRoot = truncate . sqrt . fromIntegral
