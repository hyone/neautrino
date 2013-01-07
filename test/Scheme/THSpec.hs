{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Scheme.THSpec ( spec ) where

import Test.Hspec

import Scheme.Eval (initEnv)
import Scheme.Type (LispVal(..))
import Scheme.TH

  
spec :: Spec
spec =
  describe "Scheme.HT" $ do
    describe "parseScheme" $
      it "should parse s-expression and generate abtract syntax tree" $ do
        env <- initEnv
        [parseScheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldBe` List [ Atom "begin"
                           , List [Atom "define", Atom "x", Integer 5]
                           , List [Atom "set!", Atom "x", Integer 9]
                           , Atom "x"]

    describe "parse" $
      it "should evaluate scheme s-expression" $
        pending "not yet work because Data typeclass for necessary Data has not implmented."


main :: IO ()
main = hspec spec
