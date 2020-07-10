module HowManyDigits where

f x=read$show(x-1)++(replicate(x-1)'8')++"9"

