{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Neautrino.THSpec ( spec ) where

import Test.Hspec

import Neautrino (evalAST, initEnv, scheme)
import Neautrino.Type (LispVal(..))

  
spec :: Spec
spec =
  describe "Neautrino.HT" $
    describe "scheme" $ do
      it "should parse s-expression and generate abtract syntax tree" $
        [scheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldBe` List [ Atom "begin"
                           , List [Atom "define", Atom "x", Integer 5]
                           , List [Atom "set!", Atom "x", Integer 9]
                           , Atom "x"]

      it "should parse vector literal" $
          pending "raise error on Data.Data.toConstr(Array)"
          -- evalAST env [scheme| #(1 2 3) |]

main :: IO ()
main = hspec spec
