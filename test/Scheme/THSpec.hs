{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Scheme.THSpec ( spec ) where

import Test.Hspec

import Scheme.Eval (initEnv)
import Scheme.Type (LispVal(..))
import Scheme.TH (scheme)

  
spec :: Spec
spec =
  describe "Scheme.HT" $
    describe "scheme" $
      it "should parse s-expression and generate abtract syntax tree" $ do
        env <- initEnv
        [scheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldBe` List [ Atom "begin"
                           , List [Atom "define", Atom "x", Integer 5]
                           , List [Atom "set!", Atom "x", Integer 9]
                           , Atom "x"]

main :: IO ()
main = hspec spec
