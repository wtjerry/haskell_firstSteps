module TypeClass where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

demoPerson = Person {firstName = "Michael", lastName = "Diamond", age = 43}
s = show demoPerson
-- try out: "demo person is ++ show demoPerson"

demoPerson2 = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

