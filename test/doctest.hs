module Main where

import Test.DocTest

srcdir = "src" :: String

main :: IO ()
main = do
  doctest [ "-i" ++ srcdir, srcdir ++ "/Neautrino/Function/Equal.hs" ]
