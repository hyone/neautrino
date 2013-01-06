module Scheme.Eval
  ( apply
  , eval
  , evalBody
  , evalString
  , initEnv
  , runOne
  , runRepl
  ) where

import Scheme.Type (LispVal(..), PrimitiveFunc, IOFunc)
import Scheme.Env (Env, primitiveEnv, bindVars, getVar, setVar)
import Scheme.Error
import Scheme.Load (load, loadFrom, loadLibrary)
import Scheme.Parser (readExpr)
import Scheme.Syntax
import Scheme.Util (until_)

import Control.Monad (liftM, unless)
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)


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
-- special forms
eval env (List (Atom "define" : exps))   = defineForm env exps
eval env (List (Atom "lambda" : exps))   = lambdaForm env exps
eval env (List [Atom "quote", exp])      = quoteForm env exp
eval env (List [Atom "quasiquote", exp]) = quasiquoteForm env exp
eval env (List [Atom "set!", Atom var, form])    = eval env form >>= setVar env var
eval env (List [Atom "load", String filename])   = load env filename
eval env (List [Atom "if", p, thenExp, elseExp]) = ifForm env p thenExp elseExp
eval env (List (Atom "let" : exps))      = letForm env exps
eval env (List (Atom "begin" : exps))    = evalBody env exps
eval env (List (Atom "cond" : exps))     = condForm env exps
eval env (List (Atom "case" : p : exps)) = caseForm env p exps
eval _   val@(List [Atom "unquote", _])  =
  throwError $ DefaultError ("unquote appeared outside quasiquote: " ++ show val)
-- function application
eval env (List (func : args)) = applyFunc env func args
-- or error
eval _   badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


-- | apply function to arguments
apply :: LispVal -> IOFunc
apply (PrimitiveFunc func)   args = liftThrowsError (func args)
apply (IOPrimitiveFunc func) args = func args
apply (Func params varargs body closure) args =
    if length params /= length args && isNothing varargs
    then throwError $ NumArgsError (length params) args
    else liftIO (bindVars closure (zip params args))
         >>= bindVarArgs varargs
         >>= (evalBody `flip` body)
  where
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

    bindVarArgs :: Maybe String -> Env -> IOThrowsError Env
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env
apply notFunc _ = throwError $ NotFunctionError "invalid application" (show notFunc)

applyFunc :: Env -> LispVal -> IOFunc
applyFunc env func args = do
  func' <- eval env func
  vals  <- mapM (eval env) args
  apply func' vals

-- evaluate list of expressions and returns the value from last expression
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
  loadLibrary env "init"
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
