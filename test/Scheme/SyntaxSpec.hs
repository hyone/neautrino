{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Scheme.SyntaxSpec (spec) where

import Test.Hspec
import Scheme.TestUtil (evalAST, shouldReturnT)

import Scheme.Eval (initEnv)
import Scheme.Type (LispVal(..))
import Scheme.TH (parseScheme)

  
spec :: Spec
spec =
  describe "Scheme.Syntax" $ do
    describe "begin" $ do
      it "evaluates multi expressions sequentially." $ do
        env <- initEnv
        evalAST env [parseScheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldReturnT` Integer 9

    describe "let" $ do
      it "should treat lexical bindings: (let ((a 3) (b 5)) a (+ a b))" $ do
        env <- initEnv
        _   <- evalAST env [parseScheme|
          (define a 1)
        |]
        evalAST env [parseScheme|
          (let ((a 3) (b 5)) a (+ a b))
        |] `shouldReturnT` Integer 8
        evalAST env (Atom "a") `shouldReturnT` Integer 1

      it "should return #undef at: (let ())" $ do
        env <- initEnv
        evalAST env [parseScheme| (let ()) |]
          `shouldReturnT` Undefined


main :: IO ()
main = hspec spec
