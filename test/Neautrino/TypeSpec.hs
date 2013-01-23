module Neautrino.TypeSpec (spec) where

import Test.Hspec

import Neautrino.Type
import Data.Array (array, listArray)


spec :: Spec
spec =
  describe "Neautrino.Type" $ do
    describe "vector" $ do
      it "should create Vector" $
        vector [Integer 1, Character 'c', Atom "a"]
          `shouldBe` (Vector $ listArray (0, 2) [Integer 1, Character 'c', Atom "a"])

      it "should create empty Vector" $
        vector [] `shouldBe` (Vector $ array (1, 0) [])

main :: IO ()
main = hspec spec
