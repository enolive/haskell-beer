module DemoSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Demo Test for the Framework" $
  context "something" $ do
    it "should work" $ 
      True `shouldBe` False
    it "should work with PBT" $ property $
      \x -> x `shouldBe` False
      
