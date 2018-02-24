import Test.HUnit
import FirstSteps

emptyStringsAreEq = TestCase (assertEqual "for both empty" True (areStringEq "" ""))
aAndbStringsAreNotEq = TestCase (assertEqual "for 'a' and 'b'" False (areStringEq "a" "b"))
aAndaStringsAreEq = TestCase (assertEqual "for 'a' and 'a'" True (areStringEq "a" "a"))
abcAndabcStringsAreEq = TestCase (assertEqual "for 'abc' and 'abc'" True (areStringEq "abc" "abc"))

tests = TestList [
  TestLabel "emptyStringsAreEq" emptyStringsAreEq,
  TestLabel "aAndbStringsAreNotEq" aAndbStringsAreNotEq,
  TestLabel "aAndaStringsAreEq" aAndaStringsAreEq,
  TestLabel "abcAndabcStringsAreEq" abcAndabcStringsAreEq]
