module Neautrino.Eval
  ( apply
  , eval
  , evalBody
  , evalString
  , initEnv
  , runOne
  , runRepl
  ) where

import Neautrino.Type (LispVal(..), EvalExprMonad, liftEvalExprM, runEvalExprMonad)
import Neautrino.Env (Env, bindVars, getVar, nullEnv)
import Neautrino.Error
import Neautrino.Function (primitiveFuncs, ioPrimitiveFuncs)
import Neautrino.Load (loadFrom, loadLibrary)
import Neautrino.Parser (readExpr)
import Neautrino.Syntax (primitiveSyntaxes)
import Neautrino.Util (until_)

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (isNothing)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)


-- primitive variables
primitiveEnv :: IO Env
primitiveEnv =
    nullEnv >>=
      flip bindVars
        (  map (buildSyntax Syntax) primitiveSyntaxes
        ++ map (buildFunc IOPrimitiveFunc) ioPrimitiveFuncs
        ++ map (buildFunc PrimitiveFunc) primitiveFuncs )
  where
    buildSyntax :: (a -> b -> c) -> (a, b) -> (a, c)
    buildSyntax constructor (var, handler) = (var, constructor var handler)

    buildFunc :: (a -> b) -> (c, a) -> (c, b)
    buildFunc constructor (var, func) = (var, constructor func)


-- | evaluate abstract syntax tree to value
eval :: LispVal -> EvalExprMonad LispVal
-- literal
eval val@(Character _) = return val
eval val@(String _)    = return val
eval val@(Integer _)   = return val
eval val@(Float _)     = return val
eval val@(Ratio _)     = return val
eval val@(Complex _)   = return val
eval val@(Bool _)      = return val
eval val@(Vector _)    = return val
-- variable
eval (Atom var) = getVar var
-- special form or function application
eval (List (procedure : args)) = applyProcedure procedure args
-- or error
eval badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


-- | apply function to argument list
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc   func) args = liftThrowsError (func args)
apply (IOPrimitiveFunc func) args = func args
apply (Func params varargs body env) args =
    if length params /= length args && isNothing varargs
    then throwError $ NumArgsError (length params) args
    else do
      liftIO (bindVars env (zip params args))
        >>= bindVarArgs varargs
        >>= runReaderT (evalBody body)
  where
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

    bindVarArgs :: Maybe String -> Env -> IOThrowsError Env
    bindVarArgs arg env' = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env'
apply notFunc _ = throwError $ NotFunctionError "invalid application" (show notFunc)


applyProcedure :: LispVal -> [LispVal] -> EvalExprMonad LispVal
applyProcedure procedure args = do
    f    <- eval procedure
    case f of
      (Syntax _ handler) -> handler args
      _                  -> do vals <- mapM eval args
                               lift $ apply f vals


-- | evaluate list of expressions and returns the value from last expression
evalBody :: [LispVal] -> EvalExprMonad LispVal
evalBody = liftM last . mapM eval


evalStringAST :: Env -> String -> IO (ThrowsError LispVal)
evalStringAST env expr = runEvalExprMonad env $ do
  parsed <- liftThrowsError $ readExpr expr
  result <- eval parsed
  return result

-- | eval String and return its result as String
evalString :: Env -> String -> IO String
evalString env expr = do
  result <- evalStringAST env expr
  return $ extractValue result

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn 


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout


-- | init environment and load initial scheme libraries.
initEnv :: IO Env
initEnv = do
  env <- primitiveEnv
  _   <- loadLibrary env "init"
  return env

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
    loop env = until_ (== "quit") (readPrompt "neautrino> ") (evalAndPrint env)
