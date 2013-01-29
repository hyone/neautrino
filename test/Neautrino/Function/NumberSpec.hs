module Neautrino.Function.NumberSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Neautrino.HspecHelper (shouldBeT, shouldErrorT)

import Neautrino.Type
import Neautrino.Error
import Neautrino.Function.Number
import Data.Complex (Complex(..))
import Data.Ratio ((%))


spec :: Spec
spec =
  describe "Neautrino.Function.Number" $ do
    describe "isNumber" $ do
      it "should be True with 3" $
        isNumber (Integer 3) `shouldBe` True

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
         isInteger (Integer 5) `shouldBe` True

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


    describe "exactToInexact" $ do
      it "Integer 5 should be Float 5.0" $
         exactToInexact [Integer 5] `shouldBeT` Float 5.0

      it "Ratio 5/2 should be Float 2.5" $
         exactToInexact [Ratio (5 % 2)] `shouldBeT` Float 2.5

      it "Float 5.0 should be Float 5.0" $
         exactToInexact [Float 5] `shouldBeT` Float 5.0

    describe "inexactToExact" $ do
      it "Float 5.0 should be Integer 5" $
         inexactToExact [Float 5.0] `shouldBeT` Integer 5

      it "Integet 5 should be Integer 5" $
         inexactToExact [Integer 5] `shouldBeT` Integer 5


    describe "numberAdd" $ do
      it "should add more than 2 args" $
         numberAdd [Integer 5, Integer 3, Integer 7] `shouldBeT` Integer 15

      it "should add Float numbers" $
         numberAdd [Float 5.3, Float 3.1, Float 7.5] `shouldBeT` Float 15.9

      it "should add Rational numbers" $
         numberAdd [Ratio (1 % 3), Ratio (3 % 5), Ratio (2 % 7)] `shouldBeT` Ratio (128 % 105)

      it "should add Complex numbers" $
         numberAdd [Complex (2.3 :+ 5.2), Complex (1.5 :+ 7.5)] `shouldBeT` Complex (3.8 :+ 12.7)

      it "should return identity number with no args" $
         numberAdd [] `shouldBeT` Integer 0

    describe "numberSub" $ do
      it "should substract numbers" $
         numberSub [Integer 5, Integer 3, Integer 7] `shouldBeT` Integer (-5)

      it "should error when no args" $
         numberSub [] `shouldErrorT` NumArgsError 1 []

    describe "numberMul" $ do
      it "should multiple numbers" $
         numberMul [Integer 5, Integer 3, Integer 7] `shouldBeT` Integer 105

      it "should return identity number with no args" $
         numberMul [] `shouldBeT` Integer 1

    describe "numberDiv" $ do
      it "should divide numbers" $
         numberDiv [Integer 5, Integer 3] `shouldBeT` Ratio (5 % 3)

      it "should error when divided by zero" $
         numberDiv [Float 5.2, Float 0.0] `shouldErrorT` DefaultError undefined

      it "should error when no args" $
         numberDiv [] `shouldErrorT` NumArgsError 1 []


main :: IO ()
main = hspec spec
