{-# LANGUAGE FlexibleInstances #-}
module Scheme.FunctionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Data.Array (Array, listArray)
import Data.Complex (Complex(..), imagPart)
import Data.Ratio ((%))

import Scheme.Function
import Scheme.Type


instance Arbitrary (Array Int LispVal) where
  arbitrary = do
    xs <- arbitrary :: Gen [LispVal]
    return $ listArray (0, length xs - 1) xs

instance Arbitrary LispVal where
  arbitrary = oneof [
      liftM Bool arbitrary
    , liftM Character arbitrary
    , liftM String arbitrary
    , liftM Number arbitrary
    , liftM Float arbitrary
    , liftM Ratio arbitrary
    , liftM Complex arbitrary
    , liftM List arbitrary
    , liftM2 Pair arbitrary arbitrary
    , liftM Vector arbitrary
    , return Undefined
    ]
    

spec :: Spec
spec =
  describe "Scheme.Function" $ do
    describe "isNumber" $ do
      it "should be True with 3" $
        isNumber (Number 3) `shouldBe` True

      it "should be True with 3.2+5i" $
        isNumber (Complex (3.2 :+ 5)) `shouldBe` True

      it "should be False with \"hello\"" $
        isNumber (String "hello") `shouldBe` False

      prop "should be isReal or isComplex" $ \x ->
        isNumber x == isReal x || isComplex x

    describe "isReal" $ do
      it "should be True with 3/8" $
         isReal (Ratio (3 % 8)) `shouldBe` True

      it "should be True with 3.2+0i" $
         isReal (Complex (3.2 :+ 0)) `shouldBe` True

      it "should be False with 3.2+3i" $
         isReal (Complex (3.2 :+ 3)) `shouldBe` False

      prop "should be subset of IsNumber" $ \x ->
          (isReal x && isNumber x) || True

    describe "isInteger" $ do
      it "should be True with 5" $
         isInteger (Number 5) `shouldBe` True

      it "should be True with 3.0" $
         isInteger (Float 3.2) `shouldBe` False

      it "should be True with 9/3" $
         isInteger (Ratio (9 % 3)) `shouldBe` True

      it "should be True with 3+0i" $
         isInteger (Complex (3 :+ 0)) `shouldBe` True

      it "should be False with 3.2" $
         isInteger (Float 3.2) `shouldBe` False

      it "should be False with 3.2+0i" $
         isInteger (Complex (3.2 :+ 0)) `shouldBe` False

      prop "should be subset of IsReal" $ \x ->
          (isInteger x && isReal x) || True


main :: IO ()
main = hspec spec
