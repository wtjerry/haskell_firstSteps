module PartiallyAppliedFunc where

half = flip div 2

appendNewLine [] = "\n"
appendNewLine (x:xs) = x:appendNewLine(xs)
