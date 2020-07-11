module MeetingSpec where

import Data.Char
import Data.List
import Meeting
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "meeting" $ do
    it "Basic Tests" $
      do
        shouldEqual
          "Alexis:Wahl;John:Bell;Victoria:Schwarz;Abba:Dorny;Grace:Meta;Ann:Arno;Madison:STAN;Alex:Cornwell;Lewis:Kern;Megan:Stan;Alex:Korn"
          "(ARNO, ANN)(BELL, JOHN)(CORNWELL, ALEX)(DORNY, ABBA)(KERN, LEWIS)(KORN, ALEX)(META, GRACE)(SCHWARZ, VICTORIA)(STAN, MADISON)(STAN, MEGAN)(WAHL, ALEXIS)"

        shouldEqual
          "John:Gates;Michael:Wahl;Megan:Bell;Paul:Dorries;James:Dorny;Lewis:Steve;Alex:Meta;Elizabeth:Russel;Anna:Korn;Ann:Kern;Amber:Cornwell"
          "(BELL, MEGAN)(CORNWELL, AMBER)(DORNY, JAMES)(DORRIES, PAUL)(GATES, JOHN)(KERN, ANN)(KORN, ANNA)(META, ALEX)(RUSSEL, ELIZABETH)(STEVE, LEWIS)(WAHL, MICHAEL)"

        shouldEqual
          "Alex:Arno;Alissa:Cornwell;Sarah:Bell;Andrew:Dorries;Ann:Kern;Haley:Arno;Paul:Dorny;Madison:Kern"
          "(ARNO, ALEX)(ARNO, HALEY)(BELL, SARAH)(CORNWELL, ALISSA)(DORNY, PAUL)(DORRIES, ANDREW)(KERN, ANN)(KERN, MADISON)"

        shouldEqual
          "Anna:Wahl;Grace:Gates;James:Russell;Elizabeth:Rudd;Victoria:STAN;Jacob:Wahl;Alex:Wahl;Antony:Gates;Alissa:Meta;Megan:Bell;Amandy:Stan;Anna:Steve"
          "(BELL, MEGAN)(GATES, ANTONY)(GATES, GRACE)(META, ALISSA)(RUDD, ELIZABETH)(RUSSELL, JAMES)(STAN, AMANDY)(STAN, VICTORIA)(STEVE, ANNA)(WAHL, ALEX)(WAHL, ANNA)(WAHL, JACOB)"

        shouldEqual
          "Ann:Russel;John:Gates;Paul:Wahl;Alex:Tolkien;Ann:Bell;Lewis:Kern;Sarah:Rudd;Sydney:Korn;Madison:Meta"
          "(BELL, ANN)(GATES, JOHN)(KERN, LEWIS)(KORN, SYDNEY)(META, MADISON)(RUDD, SARAH)(RUSSEL, ANN)(TOLKIEN, ALEX)(WAHL, PAUL)"

        shouldEqual
          "Paul:Arno;Matthew:Schwarz;Amandy:Meta;Grace:Meta;Ann:Arno;Alex:Schwarz;Jacob:Rudd;Amber:Cornwell"
          "(ARNO, ANN)(ARNO, PAUL)(CORNWELL, AMBER)(META, AMANDY)(META, GRACE)(RUDD, JACOB)(SCHWARZ, ALEX)(SCHWARZ, MATTHEW)"

        shouldEqual
          "Abba:Bell;Lewis:Cornwell;Jacob:STAN;Matthew:Schwarz;Ann:STAN;Sophia:Gates;Victoria:Korn;Anna:Bell;Paul:Kern;Alissa:Tolkien"
          "(BELL, ABBA)(BELL, ANNA)(CORNWELL, LEWIS)(GATES, SOPHIA)(KERN, PAUL)(KORN, VICTORIA)(SCHWARZ, MATTHEW)(STAN, ANN)(STAN, JACOB)(TOLKIEN, ALISSA)"

        shouldEqual
          "Victoria:Thorensen;Ann:Arno;Alexis:Wahl;Emily:Stan;Anna:STAN;Jacob:Korn;Sophia:Gates;Amber:Kern"
          "(ARNO, ANN)(GATES, SOPHIA)(KERN, AMBER)(KORN, JACOB)(STAN, ANNA)(STAN, EMILY)(THORENSEN, VICTORIA)(WAHL, ALEXIS)"

        shouldEqual
          "Andrew:Arno;Jacob:Russell;Anna:STAN;Antony:Gates;Amber:Korn;Lewis:Dorries;Ann:Burroughs;Alex:Kern;Anna:Arno;Elizabeth:Kern;John:Schwarz;Sarah:STAN"
          "(ARNO, ANDREW)(ARNO, ANNA)(BURROUGHS, ANN)(DORRIES, LEWIS)(GATES, ANTONY)(KERN, ALEX)(KERN, ELIZABETH)(KORN, AMBER)(RUSSELL, JACOB)(SCHWARZ, JOHN)(STAN, ANNA)(STAN, SARAH)"

        shouldEqual
          "Megan:Wahl;Alexis:Arno;Alex:Wahl;Grace:STAN;Amber:Kern;Amandy:Schwarz;Alissa:Stan;Paul:Kern;Ann:Meta;Lewis:Burroughs;Andrew:Bell"
          "(ARNO, ALEXIS)(BELL, ANDREW)(BURROUGHS, LEWIS)(KERN, AMBER)(KERN, PAUL)(META, ANN)(SCHWARZ, AMANDY)(STAN, ALISSA)(STAN, GRACE)(WAHL, ALEX)(WAHL, MEGAN)"

    modifyMaxSize (const 50) $
      it "Random test" $
        property $
          forAll
            randomLengthShuffledList
            ( \l ->
                let input = toInput l
                    expectation = toExpectedOutput l
                 in (meeting input) == expectation
            )

shouldEqual :: String -> String -> Expectation
shouldEqual input expected = (meeting input) `shouldBe` expected

firstNameLastNameTuples :: Gen (String, String)
firstNameLastNameTuples = (,) <$> (elements firstNames) <*> (elements lastNames)

randomLengthShuffledList :: Gen [(String, String)]
randomLengthShuffledList = do
  l <- choose (8, 20)
  vectorOf l firstNameLastNameTuples

toInput :: [(String, String)] -> String
toInput =
  concat
    . intersperse ";"
    . (map (\(fn, ln) -> fn ++ ":" ++ ln))

toExpectedOutput :: [(String, String)] -> String
toExpectedOutput =
  concat
    . (sortBy (compare))
    . ((map . map) toUpper)
    . (map (\(fn, ln) -> "(" ++ ln ++ ", " ++ fn ++ ")"))

firstNames :: [String]
firstNames = ["Emily", "Sophia", "Anna", "Anna", "Sarah", "Michael", "Jacob", "Alex", "Alex", "Alex", "Antony", "John", "Matthew", "Andrew", "Paul", "Paul", "Ann", "Ann", "Ann", "Ann", "Robert", "Megan", "Alissa", "Alexis", "Grace", "Madison", "Elizabeth", "James", "Amandy", "Abba", "Victoria", "Amber", "Sydney", "Haley", "Lewis"]

lastNames :: [String]
lastNames = ["Korn", "Arno", "Arno", "Bell", "Bell", "Kern", "Kern", "Kern", "Russel", "Meta", "Meta", "Meta", "Cornwell", "Cornwell", "Wahl", "Wahl", "Wahl", "Wahl", "Dorny", "Dorries", "Stan", "STAN", "STAN", "Thorensen", "Schwarz", "Schwarz", "Gates", "Steve", "Tolkien", "Burroughs", "Gates", "Bell", "Korn", "Russell", "Rudd"]

