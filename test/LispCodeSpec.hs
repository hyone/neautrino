module LispCodeSpec (specs) where

import Test.Hspec
import Scheme.Eval (initEnv, evalString)


specs :: Spec
specs = do
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
        evalString env "a" `shouldReturn` "#\\c"

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


main :: IO ()
main = hspec specs
