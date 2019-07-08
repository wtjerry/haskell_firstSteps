module ValidationWithApplicative where

import Control.Applicative

data Person = MkP String Integer deriving (Show)

data PersonInvalid = NameEmpty | AgeToLow deriving (Show)

nameOkay :: String -> Either [PersonInvalid] String
nameOkay "" = Left [NameEmpty]
nameOkay n = Right n

ageOkay :: Integer -> Either [PersonInvalid] Integer
ageOkay a
   | a < 0 = Left [AgeToLow]
   | otherwise = Right a

mkPerson :: String -> Integer -> Either [PersonInvalid] Person
mkPerson name age = liftA2 MkP(nameOkay name) (ageOkay age)

