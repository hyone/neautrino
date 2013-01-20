module Neautrino.EvalSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (shouldReturnT)

import Neautrino (evalAST, initEnv)
import Neautrino.Env (bindVars)
import Neautrino.Type (LispVal(..))
import Data.Array (listArray)

  
spec :: Spec
spec =
  describe "Neautrino.Eval" $ do
    describe "eval" $ do
      describe "self evaluating form" $ 
        it "should return a vector as it is" $ do
          env <- initEnv
          let vec = Vector $ listArray (0, 2) [Atom "a", Atom "b", Atom "c"]
          evalAST env vec `shouldReturnT` vec

    describe "evalSyntacticClosure" $ do
      it "should evaluate expr in own environment" $ do
        env1 <- initEnv >>= flip bindVars [("a", Integer 1)]
        env2 <- bindVars env1 [("a", Integer 99)]
        let synClosure = SyntacticClosure env2 [] (Atom "a")
        evalAST env1 synClosure `shouldReturnT` Integer 99

      it "should treet freevars for escaping own environment" $ do
        env1 <- initEnv >>= flip bindVars [("a", Integer 1)]
        env2 <- bindVars env1 [("a", Integer 99)]
        let synClosure = SyntacticClosure env2 ["a"] (Atom "a")
        evalAST env1 synClosure `shouldReturnT` Integer 1

main :: IO ()
main = hspec spec
