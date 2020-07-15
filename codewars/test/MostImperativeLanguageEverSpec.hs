module MostImperativeLanguageEverSpec where

import MostImperativeLanguageEver
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "my example" $ do
    it "can return lit" $ do
      (def $ return (lit 2))
      `shouldBe` 2

    it "can return var" $ do
      (def $ do
        a <- var 2
        return a)
      `shouldBe` 2

    it "can add with lit" $ do
      (def $ do
        a <- var 1
        a += lit 2
        return a)
      `shouldBe` 3

    it "can add with var" $ do
      (def $ do
        a <- var 1
        b <- var 2
        a += b
        return a)
      `shouldBe` 3

    it "can add twice" $ do
      (def $ do
        a <- var 1
        b <- var 2
        a += b
        a += b
        return a)
      `shouldBe` 5

    it "can sub" $ do
      (def $ do
        a <- var 1
        b <- var 2
        a += b
        a -= lit 10
        return a)
      `shouldBe` (-7)

    it "can mult" $ do
      (def $ do
        a <- var 1
        b <- var 2
        a += b
        a *= lit 10
        return a)
      `shouldBe` 30

    it "can do a simple loop" $ do
      (simpleLoop 1) `shouldBe` 1
      (simpleLoop 2) `shouldBe` 2
      (simpleLoop 3) `shouldBe` 3
      (simpleLoop 4) `shouldBe` 4

  describe "factorial" $ do
    it "should return the same as the functional one" $ do
      property $ \x -> factorial x `shouldBe` foldr (*) 1 [1 .. x]

  describe "howManyBetween" $ do
    it "should return the same as the functional one" $ do
      property $ \from to ->
        howManyBetween from to `shouldBe` (max 0 $ to - from - 1 :: Integer)
  

simpleLoop :: Integer -> Integer
simpleLoop n = def $ do
  result <- var 0
  i <- var n
  while i (>0) $ do
    result += lit 1
    i -= lit 1
  return result

-- foldr (*) 1
factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i <- var n
  while i (> 0) $ do
    result *= i
    i -= lit 1
  return result

-- ((max 0 . subtract 1) .) . subtract
howManyBetween :: Integer -> Integer -> Integer
howManyBetween c n = def $ do
  result <- var 0
  i <- var (c + 1)
  while i (< n) $ do
    result += lit 1
    i += lit 1
  return result
