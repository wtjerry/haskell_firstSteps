{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataTypesALaCarteForkSpec where

import Control.Monad
import Data.Typeable
import DataTypesALaCarteFork hiding (expr, expr2)
import GHC.Generics hiding ((:+:))
import Test.Hspec
import Test.QuickCheck

expr1 :: Expr (Add :+: Lit)
expr1 = add (lit 5) (lit 6)

expr2 :: Expr (Add :+: Lit :+: Mult)
expr2 = mult (add (lit 5) (lit 6)) (lit 2)

data E = LitE Int | AddE E E | MultE E E
  deriving (Show, Typeable, Generic)

trans :: E -> Expr (Lit :+: Add :+: Mult)
trans (LitE n) = lit n
trans (AddE a b) = add (trans a) (trans b)
trans (MultE a b) = mult (trans a) (trans b)

evalE :: E -> Int
evalE (LitE n) = n
evalE (AddE a b) = evalE a + evalE b
evalE (MultE a b) = evalE a * evalE b

renderE :: E -> String
renderE (LitE n) = show n
renderE (AddE a b) = brackets $ renderE a ++ "+" ++ renderE b
renderE (MultE a b) = brackets $ renderE a ++ "*" ++ renderE b

brackets :: String -> String
brackets s = "(" ++ s ++ ")"

instance Arbitrary E where
  arbitrary = sizedTree
  shrink = genericShrink

sizedTree :: Gen E
sizedTree = sized tree

tree :: Integral a => a -> Gen E
tree 0 = liftM LitE arbitrary
tree n =
  oneof
    [ liftM LitE arbitrary,
      liftM2 AddE subtree subtree,
      liftM2 MultE subtree subtree
    ]
  where
    subtree = tree (n `div` 2)

spec :: Spec
spec = do
  describe "Examples" $ do
    it "eval expr1 == 11" $ do
      eval expr1 `shouldBe` 11
    it "eval expr2 == 22" $ do
      eval expr2 `shouldBe` 22
    it "pretty expr1 == (5+6)" $ do
      pretty expr1 `shouldBe` "(5+6)"
    it "pretty expr2 == ((5+6)*2)" $ do
      pretty expr2 `shouldBe` "((5+6)*2)"
  describe "Random" $ do
    it "eval" $
      property $ do
        forAll arbitrary $ \expr ->
          eval (trans expr) `shouldBe` evalE expr
    it "pretty" $
      property $ do
        forAll arbitrary $ \expr ->
          pretty (trans expr) `shouldBe` renderE expr
