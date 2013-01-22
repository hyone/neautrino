{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Neautrino.EnvSpec (spec) where

import Test.Hspec

import Neautrino.Type (LispVal(..), runEvalExprMonad)
import Neautrino.Env

  
spec :: Spec
spec =
  describe "Neautrino.Env" $ do
    describe "unsetVar" $ do
      it "should remove var from the current env" $ do
        env <- nullEnv >>= flip bindVars [ ("a", Integer 3)
                                         , ("b", Integer 5)
                                         , ("c", Integer 6)]
        runEvalExprMonad env $ unsetVar "b"
        isBound env "b" `shouldReturn` False


main :: IO ()
main = hspec spec
