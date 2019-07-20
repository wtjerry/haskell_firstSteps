{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ReaderPractice where

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' k ((k', v):t)
  | k == k' = Just v
  | otherwise = lookup' k t

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x z

z' :: Integer -> Maybe Integer
z' k = lookup' k $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = a
fromMaybe' _ (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

justs :: [Maybe Integer]
justs = [Just 3, Just 2, Just 1]

main' :: IO ()
main' = do
  print $ sequenceA justs
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7