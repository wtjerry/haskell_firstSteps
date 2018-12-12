module BoneLength(Human(..), Gender(..), boneLength) where

data Gender = Female | Male deriving (Show)
data Human = Human Float Gender Float deriving (Show)

decreaseForAge age
    | age > 30 = 0.06 * (age - 30)
    | otherwise = 0

maleSize tbl = 69.089 + 2.238 * tbl
femaleSize tbl = 61.412 + 2.317 * tbl

boneLength :: Human -> Float
boneLength (Human age Male tbl) = (maleSize tbl) - (decreaseForAge age)
boneLength (Human age Female tbl) = femaleSize tbl - (decreaseForAge age)

