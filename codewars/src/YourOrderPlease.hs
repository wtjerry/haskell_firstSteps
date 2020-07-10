module YourOrderPlease where

import Data.Char (digitToInt, isDigit)
import Data.List (sortOn)

yourOrderPlease :: String -> String
yourOrderPlease [] = []
yourOrderPlease as = (unwords . sortOn toIndex . words) as
  where
    toIndex = digitToInt . head . filter isDigit
