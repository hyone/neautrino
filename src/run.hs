module Main where

import Neautrino.Run (runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args
