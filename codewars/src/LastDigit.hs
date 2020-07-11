module LastDigit where

lastDigit :: Integer -> Integer -> Integer
lastDigit base expnt
  | expnt == 0 = 1
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
  | otherwise = error "should not happen as lastDigitBase is a single digit Integer"
  where
    lastDigitBase = base `mod` 10
    magic x = lastDigitBase ^ (modOrM expnt x) `mod` 10

modOrM :: Integral p => p -> p -> p
modOrM x m
  | x == 0 = 0
  | md == 0 = m
  | otherwise = md
  where
    md = x `mod` m
