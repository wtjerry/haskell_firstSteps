module LastDigit where

lastDigit :: Integer -> Integer -> Integer
lastDigit base exponent
  | exponent == 0 = 1
  | lastDigitBase == 0 = 0
  | lastDigitBase == 1 = 1
  | lastDigitBase == 2 = magic 4
  | lastDigitBase == 3 = magic 4
  | lastDigitBase == 4 = magic 2
  | lastDigitBase == 5 = 5
  | lastDigitBase == 6 = 6
  | lastDigitBase == 7 = magic 4
  | lastDigitBase == 8 = magic 4
  | lastDigitBase == 9 = magic 2
  where
    lastDigitBase = base `mod` 10
    magic x = lastDigitBase ^ (modOrM exponent x) `mod` 10

modOrM x m
  | x == 0 = 0
  | md == 0 = m
  | otherwise = md
  where
    md = x `mod` m
