{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataTypesALaCarteSpec where

import DataTypesALaCarte hiding (expr, expr2)
import Test.Hspec

expr1 :: Expr (Add :+: Lit)
expr1 = add (lit 5) (lit 6)

expr2 :: Expr (Add :+: Lit :+: Mult)
expr2 = mult (add (lit 5) (lit 6)) (lit 2)

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
