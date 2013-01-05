module Scheme.EvalSpec (spec) where

import Test.Hspec
import Scheme.TestUtil (evalAST, shouldReturnT)

import Scheme.Eval (initEnv)
import Scheme.Type (LispVal(..))
import Scheme.Load (loadLibrary)

  
spec :: Spec
spec =
  describe "Scheme.Eval" $ do
    describe "begin" $ do
      it "evaluates multi expressions sequentially." $ do
        env <- initEnv
        evalAST env (List [ Atom "begin"
                          , List [Atom "define", Atom "x", Integer 5]
                          , List [Atom "set!", Atom "x", Integer 9]
                          , Atom "x"])
          `shouldReturnT` Integer 9

    describe "let" $ do
      it "should treat lexical bindings: (let ((a 3) (b 5)) a (+ a b))" $ do
        env <- initEnv
        evalAST env (List [ Atom "define", Atom "a", Integer 1 ])
        evalAST env (List [ Atom "let"
                          , List [ List [Atom "a", Integer 3]
                                 , List [Atom "b", Integer 5]]
                          , Atom "a"
                          , List [ Atom "+", Atom "a", Atom "b" ]])
          `shouldReturnT` Integer 8
        evalAST env (Atom "a") `shouldReturnT` Integer 1

      it "should return #undef at: (let ())" $ do
        env <- initEnv
        evalAST env (List [ Atom "let", List []])
          `shouldReturnT` Undefined


main :: IO ()
main = hspec spec
