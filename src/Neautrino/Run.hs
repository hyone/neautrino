module Neautrino.Run
  ( initEnv
  , runOne
  , runRepl
  ) where

import Neautrino.Env (Env, bindVars, nullEnv)
import Neautrino.Error (extractValue, liftErrorM)
import Neautrino.Eval (evalString)
import Neautrino.Function (procedures, ioProcedures)
import Neautrino.Load (loadFrom, loadLibrary)
import Neautrino.Syntax (primitiveSyntaxes)
import Neautrino.Type (LispVal(..), runEvalExprMonad)
import Neautrino.Util (until_)

import Control.Arrow (second)
import Control.Monad (unless)
import Data.Char (isSpace)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)


-- primitive variables
primitiveEnv :: IO Env
primitiveEnv =
    nullEnv >>=
      flip bindVars
        (  map (buildFunc Syntax) primitiveSyntaxes
        ++ map (buildFunc Procedure) ioProcedures
        ++ map (buildFunc Procedure . second (liftErrorM .)) procedures )
  where
    buildFunc :: (a -> b -> c) -> (a, b) -> (a, c)
    buildFunc constructor (var, func) = (var, constructor var func)


-- | init environment and load initial scheme libraries.
initEnv :: IO Env
initEnv = do
  env <- primitiveEnv
  _   <- loadLibrary env "init"
  return env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn 


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout


-- | run a script
runOne :: [String] -> IO ()
runOne args = do
  -- assign argumetns to 'args' variable
  env <- initEnv
         >>= \env' -> bindVars env' [("args", List (map String $ drop 1 args))]
  ret  <- runEvalExprMonad env $ loadFrom (head args)
  hPutStrLn stderr (extractValue ret)

-- | run Run Eval Print Loop
runRepl :: IO ()
runRepl = do env <- initEnv
             catchIOError (loop env)  
              ( \e -> unless (isEOFError e) $ ioError e )
  where
    loop :: Env -> IO ()
    loop env = until_ (== "quit") (readPrompt "neautrino> ") $ \input ->
                 -- skip empty or only white space input
                 unless (null input || all isSpace input) $
                   evalAndPrint env input
