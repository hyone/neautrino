module Neautrino.Function.EqualSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (shouldBeT)

import Neautrino.Function.Equal (eqvP, equalP)
import Neautrino.Type (LispVal(..))
import Data.Array (listArray)
import Data.Complex (Complex(..))
import Data.Ratio ((%))

  
spec :: Spec
spec =
  describe "Neautrino.Function.Equal" $ do
    describe "eqvP" $ do
      it "should be true when [True, True]" $
         eqvP [Bool True, Bool True] `shouldBeT` Bool True

      it "should be true when ['a, 'a]" $
         eqvP [Atom "foo", Atom "foo"] `shouldBeT` Bool True

      it "should be true when ['(), '()]" $
         eqvP [List [], List []] `shouldBeT` Bool True

      it "should be true when [5, 5]" $
         eqvP [Integer 5, Integer 5] `shouldBeT` Bool True

      it "should be false when [5, 5.0]" $
         eqvP [Integer 5, Float 5.0] `shouldBeT` Bool False

      it "should be true when [5, 10/2]" $ do
         pending "TODO: fix bug"
         -- eqvP [Integer 5, Ratio (10 % 2)] `shouldBeT` Bool True

      it "should be true when [5, 5+0i]" $
         eqvP [Integer 5, Complex (5 :+ 0)] `shouldBeT` Bool False

      it "should be false when [String, String]" $
         eqvP [String "hello", String "hello"] `shouldBeT` Bool False

      it "should be false when [List, List]" $
         eqvP [ List [Atom "foo", Integer 5, Character 'c']
              , List [Atom "foo", Integer 5, Character 'c']]
           `shouldBeT` Bool False

    describe "equalP" $ do
      it "should handle [Integer, Integer]" $
         equalP [Integer 5, Integer 5] `shouldBeT` Bool True

      it "should be false when [Integer, Float] though same value as numerical" $
         equalP [Integer 5, Float 5.0] `shouldBeT` Bool False

      it "should be true when [\"hello\", \"hello\"]" $
         equalP [String "hello", String "hello"] `shouldBeT` Bool True

      it "should handle [List, List]" $
         equalP [ List [Atom "foo", Integer 5, Character 'c']
                , List [Atom "foo", Integer 5, Character 'c']]
           `shouldBeT` Bool True

      it "should handle [Pair, Pair]" $
         equalP [ Pair [Atom "foo", Integer 5] (Character 'c')
                , Pair [Atom "foo", Integer 8] (Character 'c')]
           `shouldBeT` Bool False

      it "should handle [Vector, Vector]" $
         equalP [ Vector (listArray (0,1) [Atom "foo", Integer 5])
                , Vector (listArray (0,1) [Atom "foo", Integer 5])]
           `shouldBeT` Bool True


main :: IO ()
main = hspec spec
