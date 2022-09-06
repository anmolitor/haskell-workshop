module LibSpec where

import BasicPrelude
import Test.Hspec

spec :: Spec
spec = describe "addition" $ do
  it "can add 1 and 1 together" $ do
    1 + 1 `shouldBe` 2