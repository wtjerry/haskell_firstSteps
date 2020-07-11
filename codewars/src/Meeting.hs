module Meeting (meeting) where

import Data.Char
import Data.Function
import Data.List

meeting :: String -> String
meeting s =
  s
    & map toUpper
    & splitWhen (== ';')
    & map toName
    & sortBy (flip compare)
    & foldr (\x acc -> acc ++ "(" ++ x ++ ")") ""

toName :: String -> String
toName s =
  let (firstname : lastname : []) = splitWhen (== ':') s
   in lastname ++ ", " ++ firstname

-- rather use splitOn from package split, but codewars doesnt have that installed
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'
