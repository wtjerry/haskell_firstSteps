module Meeting (meeting) where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split

meeting :: String -> String
meeting s =
  s
    & map toUpper
    & splitOn ";"
    & map toName
    & sortBy (flip compare)
    & foldr (\x acc -> acc ++ "(" ++ x ++ ")") ""

toName :: String -> String
toName s =
  let (firstname : lastname : []) = splitOn ":" s
   in lastname ++ ", " ++ firstname
