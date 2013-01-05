module Main where

import Test.DocTest

main :: IO ()
main = do
  doctest ["Scheme/Function/Equal.hs"]
