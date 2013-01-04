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
                          , List [Atom "define", Atom "x", Number 5]
                          , List [Atom "set!", Atom "x", Number 9]
                          , Atom "x"])
          `shouldReturnT` Number 9


main :: IO ()
main = hspec spec
