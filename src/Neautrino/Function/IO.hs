module Neautrino.Function.IO
  ( makePort
  , closePort
  , readProc
  , writeProc
  , displayProc
  , newlineProc
  , readContents
  , readParse
  , readAll
  , currentCPUTime
  ) where 

import Neautrino.Error
import Neautrino.Type
import Neautrino.Parser (readExpr, readExprList)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.IO (IOMode, stdin, stdout, hPutStr, openFile, hClose, hGetLine)
import System.CPUTime


makePort :: IOMode -> IOProcedure
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _    badArgList        = throwError $ NumArgsError 1 badArgList


closePort :: IOProcedure
closePort [Port handle] = liftIO (hClose handle) >> return (Bool True)
closePort _             = return (Bool False)


readProc :: IOProcedure
readProc []            = readProc [Port stdin]
readProc [Port handle] = liftIO (hGetLine handle)
                         >>= liftErrorM . readExpr
readProc badArgList    = throwError $ NumArgsError 1 badArgList


writeProc :: IOProcedure
writeProc [obj]              = writeProc [obj, Port stdout]
writeProc [obj, Port handle] = liftIO $ hPutStr handle (show obj) >> return Undefined
writeProc badArgList         = throwError $ NumArgsError 2 badArgList


displayProc :: IOProcedure
displayProc [obj]                   = displayProc [obj, Port stdout]
displayProc [String s, Port handle] = liftIO $ hPutStr handle s  >> return Undefined
displayProc [obj,      Port handle] = liftIO $ hPutStr handle (show obj) >> return Undefined
displayProc badArgList              = throwError $ NumArgsError 2 badArgList


newlineProc :: IOProcedure
newlineProc []            = newlineProc [Port stdout]
newlineProc [Port handle] = liftIO $ hPutStr handle "\n" >> return Undefined
newlineProc badArgList    = throwError $ NumArgsError 1 badArgList


readContents :: IOProcedure
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgList        = throwError $ NumArgsError 1 badArgList


readParse :: String -> IOErrorM [LispVal]
readParse path = liftIO (readFile path)
                   >>= liftErrorM . readExprList

readAll :: IOProcedure
readAll [String filename] = liftM List $ readParse filename
readAll badArgList        = throwError $ NumArgsError 1 badArgList


currentCPUTime :: IOProcedure
currentCPUTime [] = do
  time <- liftIO getCPUTime
  return . Float . (/ (10^(9 :: Integer))) . fromIntegral $ time
currentCPUTime badArgList = throwError $ NumArgsError 0 badArgList
