{-# LANGUAGE FlexibleInstances #-}
module Neautrino.FunctionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Control.Monad (liftM, liftM2, void)
import Data.Array (Array, listArray)
import Data.Complex (Complex(..))
import Data.Ratio ((%))

import Neautrino (initEnv)
import Neautrino.Error (LispError(..), runErrorT)
import Neautrino.Function
import Neautrino.Type
import Neautrino.TestUtil (shouldReturnT, shouldErrorReturnT)


instance Arbitrary (Array Int LispVal) where
  arbitrary = do
    xs <- arbitrary :: Gen [LispVal]
    return $ listArray (0, length xs - 1) xs

instance Arbitrary LispVal where
  arbitrary = oneof [
      liftM Bool arbitrary
    , liftM Character arbitrary
    , liftM String arbitrary
    , liftM Integer arbitrary
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
  describe "Neautrino.Function" $ do
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

    describe "makeSyntacticClosure" $ do
      it "should be return new syntactic closure" $ do
         env        <- initEnv
         void $ runErrorT (makeSyntacticClosure [ SyntacticEnv env
                                                , List [Atom "a", Atom "b"]
                                                , Atom "hoge"])

      it "should raise NumArgsError when argnum is any other than 3" $ do
         env <- initEnv
         runErrorT (makeSyntacticClosure [ SyntacticEnv env
                                         , List [Atom "a", Atom "b"]
                                         , Atom "hoge"
                                         , Undefined ])
           `shouldErrorReturnT` NumArgsError 3 undefined

main :: IO ()
main = hspec spec
