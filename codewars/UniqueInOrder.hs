module UniqueInOrder (uniqueInOrder) where

import Data.List

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder = map head . group
