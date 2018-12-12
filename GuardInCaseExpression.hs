describeMaybeInt :: Maybe Int -> String
describeMaybeInt m = "The int " ++ case m of
    Just i | i > 0 -> "is positive."
           | i == 0 -> "is zero."
           | otherwise -> "is negative."
    Nothing -> "doesn't exist."
