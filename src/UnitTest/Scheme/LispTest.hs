{-# LANGUAGE TemplateHaskell #-}

module UnitTest.Scheme.EvalTest where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit ((@?=), Assertion)

import Scheme.Eval (initEnv, evalString)


main = defaultMain [testSuite]

testSuite :: Test
testSuite = $(testGroupGenerator)


case_Arithmetic1 :: Assertion
case_Arithmetic1 = do
  env <- initEnv
  ret <- evalString env "(+ 1 2 3)"
  ret @?= "6"

case_Arithmetic2 :: Assertion
case_Arithmetic2 = do
  env <- initEnv
  ret <- evalString env "(* 3 4 5)"
  ret @?= "60"

case_ListOperation :: Assertion
case_ListOperation = do
  env <- initEnv
  evalString env "(define a (cons 1 '(2 3)))"
  ret1 <- evalString env "(car a)"
  ret1 @?= "1"
  ret2 <- evalString env "(cdr a)"
  ret2 @?= "(2 3)"

case_Function :: Assertion
case_Function = do
  env <- initEnv
  evalString env "(define (f x y) (+ x y))"
  ret1 <- evalString env "(f 1 2)"
  ret1 @?= "3"
  ret2 <- evalString env "(f 5)"
  ret2 @?= "Expected 2 args; found values 5"
  
case_RecursiveFunction :: Assertion
case_RecursiveFunction = do
  env <- initEnv
  evalString env "(define (fact x) (if (= x 1) 1 (* x (fact (- x 1)))))"
  ret <- evalString env "(fact 10)"
  ret @?= "3628800"

case_Closure :: Assertion
case_Closure = do
  env <- initEnv
  evalString env "(define (gen-accumulator n) (lambda (i) (set! n (+ n i))))"
  evalString env "(define acc (gen-accumulator 5))"
  ret1 <- evalString env "(acc 3)"
  ret1 @?= "8"
  ret2 <- evalString env "(acc 5)"
  ret2 @?= "13"
  ret3 <- evalString env "(acc 6)"
  ret3 @?= "19"
