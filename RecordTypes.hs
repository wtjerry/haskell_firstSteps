module RecordTypes where

-- use either one of those functions to access the value
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , iceCreateFlavor :: String
                     } deriving (Show)

demoPerson = Person "hans" "muster" 42 180 "123 456" "strawberry"

demoPerson2 = Person { age=99, iceCreateFlavor="chocolate", firstName="kevin", lastName="white", height=160, phoneNumber="999 333" }

demoPerson3 = demoPerson { firstName = "bla" }
