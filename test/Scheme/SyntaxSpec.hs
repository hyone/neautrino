{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Scheme.SyntaxSpec (spec) where

import Test.Hspec
import Scheme.TestUtil (evalAST, shouldReturnT)

import Scheme.Env (getVar)
import Scheme.Eval (initEnv)
import Scheme.Type (LispVal(..))
import Scheme.TH (parseScheme)
import Control.Monad.Error (runErrorT)

  
spec :: Spec
spec =
  describe "Scheme.Syntax" $ do
    describe "define" $ do
      it "should bind value to new variable: (define a 22)" $ do
        env <- initEnv
        evalAST env [parseScheme| (define a 22) |]
        runErrorT (getVar env "a") `shouldReturnT` Integer 22

      it "should bind lambda to new variable: (define (foo x) (* x 2))" $ do
        env <- initEnv
        evalAST env [parseScheme| (define (foo x) (* x 2)) |]
        evalAST env [parseScheme| (foo 5) |]
          `shouldReturnT` Integer 10

    describe "if" $ do
      it "should evaluate \"else\" if pred is evaluated to anthing else #f: (if #f \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [parseScheme|
          (if (+ 1 2) "then" "else")
        |] `shouldReturnT` String "then"

      it "should evaluate \"then\" if pred is evaluated to '(): (if '() \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [parseScheme|
          (if '() "then" "else")
        |] `shouldReturnT` String "then"

      it "should evaluate \"else\" if pred is evaluated to true: (if #f \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [parseScheme|
          (if #f "then" "else")
        |] `shouldReturnT` String "else"

    describe "quote" $ do
      it "should stay an expression on uneval state" $ do
        env <- initEnv
        evalAST env [parseScheme|
          '(1 2 3)
        |] `shouldReturnT` List [Integer 1, Integer 2, Integer 3]

    describe "quasiquote" $ do
      it "quote expression but handle unquote" $ do
        env <- initEnv
        evalAST env [parseScheme| (define a 22) |]
        evalAST env [parseScheme|
          `(1 ,a 3)
        |] `shouldReturnT` List [Integer 1, Integer 22, Integer 3]

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

    describe "cond" $ do
      it "should evaluate expression that in the alist pred is evaluated to true" $ do
        env <- initEnv
        _   <- evalAST env [parseScheme|
          (define (foo n)
            (cond
              ((= n 0) "a")
              ((= n 1) "b")
              (else "z")))
        |]
        evalAST env [parseScheme| (foo 0) |] `shouldReturnT` String "a"
        evalAST env [parseScheme| (foo 1) |] `shouldReturnT` String "b"

      it "should evaluate else expression when nothing that pred is evaluted to true." $ do
        env <- initEnv
        _   <- evalAST env [parseScheme|
          (define (foo n)
            (cond
              ((= n 0) "a")
              ((= n 1) "b")
              (else "z")))
        |]
        evalAST env [parseScheme| (foo 9) |] `shouldReturnT` String "z"
 	
    describe "case" $ do
      it "should evaluate expression that in the alist the evaluated value is matched" $ do
        env <- initEnv
        evalAST env [parseScheme|
          (case (* 2 3)
            ((2 3 5 7) 'prime)
            ((1 4 6 8 9) 'composite))
        |] `shouldReturnT` Atom "composite"


main :: IO ()
main = hspec spec
