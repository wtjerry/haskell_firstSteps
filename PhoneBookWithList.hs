module PhoneBookWithList where

data Entry = Entry String Integer deriving (Show)
type PhoneBook = [Entry]

getNumber :: PhoneBook -> String -> Maybe Integer
getNumber [] _  = Nothing
getNumber ((Entry name num):xs) searchName
    | name == searchName = Just num
    | otherwise = getNumber xs searchName

addEntry :: PhoneBook -> Entry -> PhoneBook
addEntry pb e = e:pb


demoPhoneBook = [ (Entry "mike" 1),
                  (Entry "julie" 2),
                  (Entry "sarah" 3) ]
pb2 = addEntry demoPhoneBook (Entry "mia" 5)

