module HowManyDigits where

f :: Read a => Int -> a
f x=read$show(x-1)++(replicate(x-1)'8')++"9"

