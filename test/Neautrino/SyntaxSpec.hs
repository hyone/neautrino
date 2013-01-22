{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Neautrino.SyntaxSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (shouldReturnT, shouldErrorReturnT, assertNoIOErrorT)

import Neautrino (evalAST, initEnv, scheme)
import Neautrino.Env (getVar)
import Neautrino.Type (LispVal(..), runEvalExprMonad)

import Data.Array (listArray)

  
spec :: Spec
spec =
  describe "Neautrino.Syntax" $ do
    describe "define" $ do
      it "should bind value to new variable: (define a 22)" $ do
        env <- initEnv
        evalAST env [scheme| (define a 22) |]
        runEvalExprMonad env (getVar "a") `shouldReturnT` Integer 22

      it "should bind lambda to new variable: (define (foo x) (* x 2))" $ do
        env <- initEnv
        evalAST env [scheme| (define (foo x) (* x 2)) |]
        evalAST env [scheme| (foo 5) |]
          `shouldReturnT` Integer 10

    -- describe "define-macro" $ do
    --   it "should define normal macro with MIT style" $ do
    --     env <- initEnv
    --     evalAST env [scheme| (define-macro (mymacro a b) (list 'begin a b)) |]
    --     evalAST env [scheme| (mymacro 2 3) |]
    --       `shouldReturnT` Integer 3

    --   it "should define varargs macro with MIT style" $ do
    --     env <- initEnv
    --     evalAST env [scheme|
    --       (define-macro (when t . body)
    --          (list 'if t (cons 'begin body)))
    --     |]
    --     evalAST env [scheme| (when #t 2 3) |]
    --       `shouldReturnT` Integer 3

    --   it "should define normal macro with lambda expression" $ do
    --     env <- initEnv
    --     evalAST env [scheme| (define-macro mymacro (lambda (a b) (list 'begin a b))) |]
    --     evalAST env [scheme| (mymacro 2 3) |]
    --       `shouldReturnT` Integer 3

    --   it "should define varargs macro with lambda expression" $ do
    --     env <- initEnv
    --     evalAST env [scheme|
    --       (define-macro when
    --         (lambda (t . body)
    --           (list 'if t (cons 'begin body))))
    --     |]
    --     evalAST env [scheme| (when #t 2 3) |]
    --       `shouldReturnT` Integer 3

    describe "if" $ do
      it "should evaluate \"else\" when pred is evaluated to anthing else #f: (if #f \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [scheme|
          (if (+ 1 2) "then" "else")
        |] `shouldReturnT` String "then"

      it "should evaluate \"then\" when pred is evaluated to '(): (if '() \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [scheme|
          (if '() "then" "else")
        |] `shouldReturnT` String "then"

      it "should evaluate \"else\" when pred is evaluated to true: (if #f \"then\" \"else\")" $ do
        env <- initEnv
        evalAST env [scheme|
          (if #f "then" "else")
        |] `shouldReturnT` String "else"

      it "should return #undef when pred is evaluated to false and do not supply \"else\": (if #f \"then\")" $ do
        env <- initEnv
        evalAST env [scheme|
          (if #f "then")
        |] `shouldReturnT` Undefined

    describe "quote" $ do
      it "should stay an expression on uneval state" $ do
        env <- initEnv
        evalAST env [scheme|
          '(1 2 3)
        |] `shouldReturnT` List [Integer 1, Integer 2, Integer 3]

    describe "quasiquote" $ do
      describe "unquote" $ do
        it "should handle variable" $ do
          env <- initEnv
          evalAST env [scheme| (define a 22) |]
          evalAST env [scheme|
            `(1 ,a 3)
          |] `shouldReturnT` List [Integer 1, Integer 22, Integer 3]

      describe "unquote-splicing" $ do
        it "should be evaluated in list" $ do
          env <- initEnv
          evalAST env [scheme| (define a '(2 3)) |]

          evalAST env [scheme| `(1 ,@a 4) |]
            `shouldReturnT` List [Integer 1, Integer 2, Integer 3, Integer 4]

        it "should be evaluated in pair" $ do
          env <- initEnv
          evalAST env [scheme| (define a '(2 3)) |]
          evalAST env [scheme| `(1 (,@a 4) . 5) |]
            `shouldReturnT` Pair [Integer 1, List [Integer 2, Integer 3, Integer 4]]
                                 (Integer 5)

        it "should be evaluated in vector" $ do
          env <- initEnv
          evalAST env [scheme| (define a '(2 3)) |]
          evalAST env (List [ Atom "quasiquote"
                            , Vector (listArray (0, 2)
                                                [ Integer 1
                                                , List [Atom "unquote-splicing", Atom "a"]
                                                , Integer 4] )])
            `shouldReturnT`
            Vector (listArray (0, 3) [Integer 1, Integer 2, Integer 3, Integer 4])

        it "should handle expression" $ do
          env <- initEnv
          evalAST env [scheme| (define a '(2 3)) |]
          evalAST env [scheme| `(1 ,@(cdr '(0 2 3)) 4) |]
            `shouldReturnT` List [Integer 1, Integer 2, Integer 3, Integer 4]
                                                                             

    describe "begin" $ do
      it "evaluates multi expressions sequentially." $ do
        env <- initEnv
        evalAST env [scheme|
          (begin (define x 5) (set! x 9) x)
        |] `shouldReturnT` Integer 9

    describe "let" $ do
      it "should treat lexical bindings: (let ((a 3) (b 5)) a (+ a b))" $ do
        env <- initEnv
        _   <- evalAST env [scheme|
          (define a 1)
        |]
        evalAST env [scheme|
          (let ((a 3) (b (+ 1 4))) a (+ a b))
        |] `shouldReturnT` Integer 8
        evalAST env (Atom "a") `shouldReturnT` Integer 1

      it "should return #undef at: (let ())" $ do
        env <- initEnv
        evalAST env [scheme| (let ()) |]
          `shouldReturnT` Undefined

    describe "cond" $ do
      it "should evaluate expression that in the alist pred is evaluated to true" $ do
        env <- initEnv
        _   <- evalAST env [scheme|
          (define (foo n)
            (cond
              ((= n 0) "a")
              ((= n 1) "b")
              (else "z")))
        |]
        evalAST env [scheme| (foo 0) |] `shouldReturnT` String "a"
        evalAST env [scheme| (foo 1) |] `shouldReturnT` String "b"

      it "should evaluate else expression when nothing that pred is evaluted to true." $ do
        env <- initEnv
        _   <- evalAST env [scheme|
          (define (foo n)
            (cond
              ((= n 0) "a")
              ((= n 1) "b")
              (else "z")))
        |]
        evalAST env [scheme| (foo 9) |] `shouldReturnT` String "z"
 	
    describe "case" $ do
      it "should evaluate expression that in the alist the evaluated value is matched" $ do
        env <- initEnv
        evalAST env [scheme|
          (case (* 2 3)
            ((2 3 5 7) 'prime)
            ((1 4 6 8 9) 'composite))
        |] `shouldReturnT` Atom "composite"

    describe "define-syntax" $ do
      it "whose name should be referrable in macro of er-macro-transformer" $ do
        env <- initEnv
        evalAST env [scheme|
          (define-syntax foo
            (er-macro-transformer
             (lambda (expr rename compare)
               `(,(rename 'display) ,(rename 'foo)))))
        |]
        assertNoIOErrorT $ evalAST env [scheme| (foo) |]

main :: IO ()
main = hspec spec
