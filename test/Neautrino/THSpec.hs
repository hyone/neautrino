{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Neautrino.THSpec ( spec ) where

import Test.Hspec

import Neautrino.Type (LispVal(..))
import Neautrino.TH (scheme)

  
spec :: Spec
spec =
  describe "Neautrino.HT" $
    describe "scheme" $
      it "should parse s-expression and generate abtract syntax tree" $
        [scheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldBe` List [ Atom "begin"
                           , List [Atom "define", Atom "x", Integer 5]
                           , List [Atom "set!", Atom "x", Integer 9]
                           , Atom "x"]

      it "should parse vector literal" $
          peinding "raise error on Data.Data.toConstr(Array)"
          -- evalAST env [scheme| #(1 2 3) |]

main :: IO ()
main = hspec spec
