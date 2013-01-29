{-# LANGUAGE FlexibleInstances #-}
module Neautrino.FunctionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad (liftM, liftM2, void)
import Data.Array (Array, listArray)
import Data.Complex (Complex(..))
import Data.Ratio ((%))

import Neautrino (initEnv)
import Neautrino.Error (LispError(..), runErrorT)
import Neautrino.Function
import Neautrino.Type
import Neautrino.HspecHelper (shouldBeT, shouldReturnT, shouldErrorT)


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

    describe "isPair" $ do
      it "should be True with (2 . 3)" $
         isPair (Pair [Integer 2] (Integer 3)) `shouldBe` True

      it "should be True with (2)" $
         isPair (List [Integer 2]) `shouldBe` True

      it "should be False with ()" $
         isPair (List []) `shouldBe` False

    describe "isEnv" $
      it "should be True with syntactic environment and False with otherwise" $ do
         env <- initEnv
         isEnv (SyntacticEnv env) `shouldBe` True
         isEnv (List []) `shouldBe` False
         isEnv Undefined `shouldBe` False

    describe "makeSyntacticClosure" $ do
      it "should be return new syntactic closure" $ do
         env <- initEnv
         void . return $ makeSyntacticClosure
                           [ SyntacticEnv env
                           , List [Atom "a", Atom "b"]
                           , Atom "hoge"]

      it "should raise NumArgsError when argnum is any other than 3" $ do
         env <- initEnv
         makeSyntacticClosure [ SyntacticEnv env
                              , List [Atom "a", Atom "b"]
                              , Atom "hoge"
                              , Undefined ]
           `shouldErrorT` NumArgsError 3 undefined

    describe "stripSyntacticClosures" $
      it "should convert syntactic closure to normal expr" $ do
         env <- initEnv
         let expr = [List [ SyntacticClosure env [] (List [ Atom "+", Integer 3, Integer 5 ])
                           , List [ Atom "a", SyntacticClosure env [] (Atom "b") ]
                           , Atom "hoge" ]]
         stripSyntacticClosures expr
           `shouldBeT` List [ List [ Atom "+", Integer 3, Integer 5 ]
                            , List [ Atom "a", Atom "b" ]
                            , Atom "hoge" ]

    describe "identifierEqualP" $ do
      it "should be comparable between symbol and alias" $ do
         env <- initEnv
         ret <- runErrorT $
                  identifierEqualP [ SyntacticEnv env
                                   , SyntacticClosure env [] (Atom "define")
                                   , SyntacticEnv env
                                   , Atom "define" ]
         ret `shouldBeT` Bool True

      it "should be comparable between symbols that is not bounded" $ do
         env <- initEnv
         ret <- runErrorT $
                  identifierEqualP [ SyntacticEnv env
                                   , Atom "no_such_var"
                                   , SyntacticEnv env
                                   , Atom "no_such_var" ]
         ret `shouldBeT` Bool True


main :: IO ()
main = hspec spec
