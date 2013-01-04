{-# LANGUAGE DoAndIfThenElse #-}
-- | functionality to load a script file
module Scheme.Load
  ( load
  , loadLibrary
  ) where

import Control.Monad (filterM, liftM, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath (combine, addExtension, takeExtension)

import Paths_simple_scheme (getDataFileName)
import Scheme.Env (Env)
import Scheme.Error
import {-# SOURCE #-} Scheme.Eval (evalBody)
import Scheme.Function (readParse)
import Scheme.Type


systemLoadPath :: IO FilePath
systemLoadPath = getDataFileName "lib"

loadPath :: IO [FilePath]
loadPath = sequence [systemLoadPath]

fixFileExtension :: FilePath -> FilePath
fixFileExtension filename = 
  if takeExtension filename == ".scm"
  then filename
  else addExtension filename "scm"

buildSearchPaths :: FilePath -> [FilePath] -> IO [FilePath]
buildSearchPaths filename dirs =
  filterM doesFileExist $
    map (`combine` filename) dirs

findLibrary :: FilePath -> IOThrowsError FilePath
findLibrary filename = do
  dirs  <- liftIO loadPath
  paths <- liftIO $ buildSearchPaths (fixFileExtension filename) dirs
  case paths of
    (x:_) -> return x
    _     -> throwError $ DefaultError $
               "Cannot find \"" ++ filename ++ "\" in " ++ show dirs


-- | read and evaluate a file 
load :: Env -> FilePath -> IOThrowsError LispVal
load env filename = do
    let filename' = fixFileExtension filename
    if any (`isPrefixOf` filename) ["./", "../", "/"] then
      load' filename'
    else do
      path <- findLibrary filename'
      load' path
  where
    load' :: FilePath -> IOThrowsError LispVal
    load' path = do
      p <- liftIO $ doesFileExist path
      if p then
        readParse path >>= evalBody env
      else
        throwError $ DefaultError $
          "Cannot find \"" ++ filename ++ "\""

-- | load and run IOThrowsError
loadLibrary :: Env -> FilePath -> IO String
loadLibrary env = runIOThrowsError . liftM show . load env
