module Fib where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib (x-2)) + (fib (x-1))


-- usage:
-- >>> maybeFib (-1)
-- Nothing
--
-- >>> maybeFib 15
-- Just 610
maybeFib :: Integer -> Maybe Integer
maybeFib x | x < 0 = Nothing
maybeFib 0 = Just 0
maybeFib 1 = Just 1
maybeFib y = do { a <- (maybeFib (y-2))
           ; b <- (maybeFib (y-1))
           ; return (a + b) }

-- usage:
-- >>> maybeFibButReturnInt (-2)
-- -1
--
-- >>> maybeFibButReturnInt 15
-- 610
maybeFibButReturnInt x = maybe (-1) id (maybeFib x)


-- using List comprehension
fibFast = 1 : 1 : [a + b | (a, b) <- zip fibFast (tail fibFast)]

fibFastX x = head (drop (x-1) fibFast)


