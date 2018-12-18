module PartiallyAppliedFunc where

half = flip div 2
demoHalf = take 10 $ map half [1..]

squares = map (^2) [1..]
demoSquares = take 10 squares 
