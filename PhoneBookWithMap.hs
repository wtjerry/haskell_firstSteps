module PhoneBookWithMap where

import qualified Data.Map as M

type Name = String -- you may change this to (String, String) and use demoPhoneBook2
type PhoneNumber = Integer
type PhoneBook = M.Map Name PhoneNumber

addEntry :: Name -> PhoneNumber -> PhoneBook -> PhoneBook
addEntry name number pb = M.insert name number pb

getNumber :: Name -> PhoneBook -> Maybe PhoneNumber
getNumber name pb = M.lookup name pb

demoPhoneBook = M.fromList
    [ ("mike", 1),
      ("julie", 2),
      ("sarah", 3) ]

demoPhoneBook2 = M.fromList
    [ (("mike", "müller"), 1),
      (("julie", "maurer"), 2),
      (("sarah", "schneider"), 3) ]
