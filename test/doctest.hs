module Main where

import Test.DocTest

srcdir :: String
srcdir = "src"

main :: IO ()
main = do
  doctest [ "-i" ++ srcdir, srcdir ++ "/Neautrino/Function/Equal.hs" ]
