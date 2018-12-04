import Test.HUnit
import BoneLength

-- usage:
-- first open this file with ghci then
-- >>> runTestTT tests

male29 = TestCase (assertEqual "" 180.98898 (boneLength (Human 29 Male 50)))
male30 = TestCase (assertEqual "" 180.98898 (boneLength (Human 30 Male 50)))
male31 = TestCase (assertEqual "" 180.92898 (boneLength (Human 31 Male 50)))
female29 = TestCase (assertEqual "" 177.262 (boneLength (Human 29 Female 50)))
female30 = TestCase (assertEqual "" 177.262 (boneLength (Human 30 Female 50)))
female31 = TestCase (assertEqual "" 177.202 (boneLength (Human 31 Female 50)))

tests = TestList [
    TestLabel "male29" male29,
    TestLabel "male30" male30,
    TestLabel "male31" male31,
    TestLabel "female29" female29,
    TestLabel "female30" female30,
    TestLabel "female31" female31 ]

