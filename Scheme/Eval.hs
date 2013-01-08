module Scheme.Eval
  ( apply
  , eval
  , evalBody
  , evalString
  , initEnv
  , runOne
  , runRepl
  ) where

import Scheme.Type (LispVal(..), SyntaxHandler)
import Scheme.Env (Env, bindVars, getVar, nullEnv)
import Scheme.Error
import Scheme.Function (primitiveFuncs, ioPrimitiveFuncs)
import Scheme.Load (loadFrom, loadLibrary)
import Scheme.Parser (readExpr)
import Scheme.Syntax (primitiveSyntaxes)
import Scheme.Util (until_)

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (liftIO)
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
eval :: Env -> LispVal -> IOThrowsError LispVal
-- literal
eval _   val@(Character _) = return val
eval _   val@(String _)    = return val
eval _   val@(Integer _)   = return val
eval _   val@(Float _)     = return val
eval _   val@(Ratio _)     = return val
eval _   val@(Complex _)   = return val
eval _   val@(Bool _)      = return val
-- variable
eval env (Atom var) = getVar env var
-- special form or function application
eval env (List (procedure : args)) = applyProcedure env procedure args
-- or error
eval _   badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


-- | apply function to argument list
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc   func) args = liftThrowsError (func args)
apply (IOPrimitiveFunc func) args = func args
apply (Func params varargs body env) args =
    if length params /= length args && isNothing varargs
    then throwError $ NumArgsError (length params) args
    else liftIO (bindVars env (zip params args))
         >>= bindVarArgs varargs
         >>= (evalBody `flip` body)
  where
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

    bindVarArgs :: Maybe String -> Env -> IOThrowsError Env
    bindVarArgs arg env' = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env'
apply notFunc _ = throwError $ NotFunctionError "invalid application" (show notFunc)


applyProcedure :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
applyProcedure env procedure args = do
    f    <- eval env procedure
    case f of
      (Syntax _ handler) -> handler env args
      _                  -> do vals <- mapM (eval env) args
                               apply f vals


-- | evaluate list of expressions and returns the value from last expression
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
evalBody env = liftM last . mapM (eval env)


evalStringAST :: Env -> String -> IO (ThrowsError LispVal)
evalStringAST env expr = runErrorT $ do
  parsed <- liftThrowsError $ readExpr expr
  result <- eval env parsed
  return result

-- | eval String and return its result as String
evalString :: Env -> String -> IO String
evalString env expr = do
  result <- evalStringAST env expr
  return $ extractValue (fmap show result)

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
  env <- initEnv >>= bindVars `flip` [("args", List (map String $ drop 1 args))]
  runIOThrowsError $ liftM show (loadFrom env (head args))
  >>= hPutStrLn stderr

-- | run Run Eval Print Loop
runRepl :: IO ()
runRepl = do env <- initEnv
             catchIOError (loop env)  
              ( \e -> unless (isEOFError e) $ ioError e )
  where
    loop :: Env -> IO ()
    loop env = until_ (== "quit") (readPrompt "scheme> ") (evalAndPrint env)
