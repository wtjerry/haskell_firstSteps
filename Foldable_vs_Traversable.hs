instance (Monoid a, Monoid b) => Monoid (Either a b) where
  mempty = Right mempty
  mappend (Right a) (Right b) = Right (mappend a b)
  mappend (Right a) (Left _) = Right a
  mappend (Left _) (Right a) = Right a
  mappend (Left a) (Left b) = Left (mappend a b)
  

f :: Integer -> Maybe String
f 10 = Just "10"
f 11 = Just "11"
f _ = Nothing

f' :: Integer -> Either String String
f' 10 = Right "10"
f' 11 = Right "11"
f' _ = Left "bad input"

l = [ 5..15 ]


-- try the following:
--
-- traverse f l
-- --> Nothing
-- traverse f' l
-- -> Left "bad input"
--
-- foldMap f l
-- -> Just "1011"
-- foldMap f' l
-- -> Right "1011"
--
--
-- -> traverse and foldMap have the almost the same signature
-- -> but traverse is not able to preserve information because of the lacking Monoid constraint
