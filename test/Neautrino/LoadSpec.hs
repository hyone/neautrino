module Neautrino.LoadSpec (spec) where

import Test.Hspec
import Neautrino.TestUtil (shouldContain, shouldReturnT)

import Neautrino (evalAST, initEnv)
import Neautrino.Load (loadLibrary)
import Neautrino.Type (LispVal(..))


loadTest :: FilePath -> IO ()
loadTest path = do
  env <- initEnv
  loadLibrary env path `shouldReturn` "1"
  evalAST env (List [Atom "fact", Integer 5])
    `shouldReturnT` Integer 120
  
spec :: Spec
spec =
  describe "Neautrino.Load" $ do
    describe "loadLibrary" $ do
      it "handle relative path starts with \"./\"." $ 
        loadTest "./lib/test_fact.scm" 

      it "handle relative path starts with \"../\"." $ 
        loadTest "../neautrino/lib/test_fact.scm" 

      it "handle absolute path." $ 
        pending "TODO: detect automatically abosolute path of fact.scm"
        -- loadTest "/path/to/test_fact.scm" 

      it "search from load-path" $ 
        loadTest "test_fact.scm" 

      it "handle path without file extension #1: \"./lib/test_fact\"" $ 
        loadTest "./lib/test_fact" 

      it "handle path without file extension #2: \"test_fact\"" $ 
        loadTest "test_fact" 

      it "raise an error when the file did not found." $ do
        env <- initEnv
        loadLibrary env "no_such_file" >>= \got ->
          got `shouldContain` "Cannot find"

main :: IO ()
main = hspec spec
