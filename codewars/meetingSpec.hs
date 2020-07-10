module ExampleSpec where

import Test.Hspec
import Example

-- `spec` of type `Spec` must exist
spec :: Spec
spec = do
    describe "add" $ do
        it "adds Nums" $ do
            (add 1 1) `shouldBe` (2 :: Integer)

-- the following line is optional for 8.2
main = hspec spec
