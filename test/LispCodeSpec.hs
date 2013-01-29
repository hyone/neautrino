{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module LispCodeSpec (spec) where

import Test.Hspec
import Neautrino.HspecHelper (shouldReturnT)

import Neautrino (evalAST, evalString, initEnv, scheme)
import Neautrino.Type (LispVal(..))


spec :: Spec
spec = do
  describe "LispCode" $ do
    describe "arithmetic" $ do
      it "additional" $ do
        env <- initEnv
        evalString env "(+ 1 2 3)" `shouldReturn` "6"
        evalString env "(+ 4 5 6)" `shouldReturn` "15"
        -- evalString env "(+)"       `shouldReturn` "0"

      it "multiplication" $ do
        env <- initEnv
        evalString env "(* 1 2 3)" `shouldReturn` "6"
        evalString env "(* 4 5 6)" `shouldReturn` "120"
        -- evalString env "(*)"       `shouldReturn` "0"

    describe "list" $ do
      it "basic" $ do
        env <- initEnv
        evalString env "(cons 1 '(2 3))" `shouldReturn` "(1 2 3)"
        evalString env "(car '(1 2 3))"  `shouldReturn` "1"
        evalString env "(cdr '(1 2 3))"  `shouldReturn` "(2 3)"

    describe "variable" $ do
      it "basic" $ do
        env <- initEnv
        _   <- evalString env "(define a 3)"
        evalString env "a" `shouldReturn` "3"
        _   <- evalString env "(set! a #\\c)"
        evalString env "a" `shouldReturn` "c"

    describe "function" $ do
      it "basic" $ do
        env <- initEnv
        _   <- evalString env "(define (f x y) (+ x y))"
        evalString env "(f 1 2)" `shouldReturn` "3"
        evalString env "(f 5)"   `shouldReturn` "Expected 2 args; found values 5"

      it "recursive function" $ do
        env <- initEnv
        _   <- evalString env "(define (fact x) (if (= x 1) 1 (* x (fact (- x 1)))))"
        evalString env "(fact 10)" `shouldReturn` "3628800"

      it "closure" $ do
        env <- initEnv
        _   <- evalString env "(define (gen-accumulator n) (lambda (i) (set! n (+ n i))))"
        _   <- evalString env "(define acc (gen-accumulator 5))"
        evalString env "(acc 3)" `shouldReturn` "8"
        evalString env "(acc 5)" `shouldReturn` "13"
        evalString env "(acc 6)" `shouldReturn` "19"

    describe "quote" $ do
      it "quote" $ do
        env <- initEnv
        evalString env "'(1 2 3)" `shouldReturn` "(1 2 3)"

      it "quasiquote" $ do
        env <- initEnv
        _   <- evalString env "(define a 5)"
        evalString env "`(1 ,a 3)" `shouldReturn` "(1 5 3)"

    describe "macro" $ do
      describe "sc-macro-transformer" $
        it "swap!" $ do
          env <- initEnv
          evalAST env [scheme|
            (define-syntax swap! 
               (sc-macro-transformer 
                (lambda (form environment) 
                  (let ((a (make-syntactic-closure environment '() (cadr form))) 
                        (b (make-syntactic-closure environment '() (caddr form)))) 
                    `(let ((v ,a)) 
                       (set! ,a ,b) 
                       (set! ,b v))))))
          |]
          evalAST env [scheme| (begin (define a 1) (define v 99)) |]
          evalAST env [scheme| (swap! a v) |]
          evalAST env [scheme| (list a v) |] `shouldReturnT` List [Integer 99, Integer 1]

      describe "er-macro-transformer" $ 
        it "swap!" $ do
          env <- initEnv
          evalAST env [scheme|
            (define-syntax swap!
              (er-macro-transformer
               (lambda (form rename compare)
                 (let ((a (cadr form))
                       (b (caddr form)))
                   `(,(rename 'let) ((,(rename 'value) ,a))
                      (,(rename 'set!) ,a ,b)
                      (,(rename 'set!) ,b ,(rename 'value)))))))
          |]
          evalAST env [scheme| (begin (define a 1) (define v 99)) |]
          evalAST env [scheme| (swap! a v) |]
          evalAST env [scheme| (list a v) |] `shouldReturnT` List [Integer 99, Integer 1]

      describe "define non-hygienic macro" $
        it "swap!" $ do
          env <- initEnv
          evalAST env [scheme|
            (define-syntax swap! 
               (er-macro-transformer 
                (lambda (form rename compare) 
                  (let ((a (cadr form)) 
                        (b (caddr form)))
                    `(let ((v ,a)) 
                       (set! ,a ,b) 
                       (set! ,b v))))))
          |]
          evalAST env [scheme| (begin (define a 1) (define v 99)) |]
          evalAST env [scheme| (swap! a v) |]
          evalAST env [scheme| (list a v) |] `shouldReturnT` List [Integer 1, Integer 99]

      describe "syntax-rules" $
        it "incf" $ do
          env <- initEnv
          evalAST env [scheme|
            (define-syntax incf
              (syntax-rules ()
                ((_ x)
                 (incf x 1))
                ((_ x i)
                 (begin (set! x (+ x i)) x))))
          |]
          evalAST env [scheme| (define i 0) |]
          evalAST env [scheme| (incf i) |]   `shouldReturnT` Integer 1
          evalAST env [scheme| (incf i 5) |] `shouldReturnT` Integer 6
          evalAST env [scheme| i |]          `shouldReturnT` Integer 6

main :: IO ()
main = hspec spec
