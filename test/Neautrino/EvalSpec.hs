{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Neautrino.EvalSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (evalAST, shouldReturnT)

import Neautrino.Eval (initEnv)
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

main :: IO ()
main = hspec spec
