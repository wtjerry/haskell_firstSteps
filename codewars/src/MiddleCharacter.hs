module MiddleCharacter where

getMiddle :: String -> String
getMiddle [] = []
getMiddle s
  | odd l = [s !! halfL]
  | otherwise = [s !! (halfL -1), s !! halfL]
  where
    l = length s
    halfL = l `div` 2
