{-# LANGUAGE DoAndIfThenElse #-}
-- | functionality to load a script file
module Neautrino.Load
  ( load
  , loadFrom
  , loadLibrary
  ) where

import Control.Monad (filterM, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath (combine, addExtension, takeExtension)

import Paths_neautrino (getDataFileName)
import Neautrino.Env (Env)
import Neautrino.Error
import {-# SOURCE #-} Neautrino.Eval (evalBody)
import Neautrino.Function (readParse)
import Neautrino.Type


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


-- | load a file from specific path
loadFrom :: FilePath -> EvalExprMonad LispVal
loadFrom path = do
  p <- liftIO $ doesFileExist path
  if p then
    lift (readParse path) >>= evalBody
  else
    throwError $ DefaultError $
      "Cannot find \"" ++ path ++ "\""

-- | load a file from specific path or in load-path
load :: FilePath -> EvalExprMonad LispVal
load filename = do
    let filename' = fixFileExtension filename
    path <- if any (`isPrefixOf` filename') ["./", "../", "/"] then
      return filename'
    else 
      lift (findLibrary filename')
    loadFrom path

-- | load and runEvalExprMonad
loadLibrary :: Env -> FilePath -> IO String
loadLibrary env path = liftM extractValue $ runEvalExprMonad env (load path)
