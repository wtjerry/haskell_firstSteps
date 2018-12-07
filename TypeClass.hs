module TypeClass where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

demoPerson = Person {firstName = "Michael", lastName = "Diamond", age = 43}
s = show demoPerson
-- try out: "demo person is ++ show demoPerson"

demoPerson2 = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person


data Color = Green | Blue | Red | Yellow deriving (Show)

class Colorable a where
    colorOf :: a -> Color

instance Colorable Integer where
    colorOf 42 = Green
    colorOf 0 = Yellow
    colorOf i | i < 0 = Red
    colorOf _ = Blue

