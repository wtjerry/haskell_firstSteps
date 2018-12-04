module Retirement where

data Gender = Female | Male deriving (Show)
-- Human Age Gender
data Human = Human Float Gender deriving (Show)
isRetired (Human age Female) = age >= 64
isRetired (Human age Male) = age >= 65

isRetired2 h = case h of (Human age Female) -> age >= 64
                         (Human age Male) -> age >= 65

