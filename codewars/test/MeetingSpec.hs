module MeetingSpec where

import Test.Hspec

-- `spec` of type `Spec` must exist
spec :: Spec
spec = do
    describe "add" $ do
        it "adds Nums" $ do
            ((+) 1 1) `shouldBe` (2 :: Integer)

-- the following line is optional for 8.2
main = hspec spec
