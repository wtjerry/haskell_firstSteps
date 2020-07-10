module YourOrderPlease where

import Data.Char (isDigit, digitToInt)
import Data.List (sortOn)

yourOrderPlease :: String -> String
yourOrderPlease [] = []
yourOrderPlease as = (unwords . sortOn toIndex . words) as
    where toIndex = digitToInt . head . filter isDigit

