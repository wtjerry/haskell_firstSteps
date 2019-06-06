module Kata where

import Data.List

findMissingLetter :: [Char] -> Char
findMissingLetter cs@(c:_) = head $ completeList \\ cs
    where completeList = take ((length cs) + 1) [c..]

