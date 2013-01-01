module Scheme.Eval
  ( apply
  , eval
  , evalString
  , initEnv
  , runOne
  , runRepl
  ) where

import Scheme.Type
import Scheme.Env
import Scheme.Error
import Scheme.Parser (readExpr, readExprList)
import Scheme.Util (until_)
import qualified Scheme.Function as F

import Control.Monad
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError, isEOFError)


-- ----------------------------------------------------------------------------------------
-- Evaluation

eval :: Env -> LispVal -> IOThrowsError LispVal
-- literal
eval env val@(Character _) = return val
eval env val@(String _)    = return val
eval env val@(Number _)    = return val
eval env val@(Float _)     = return val
eval env val@(Ratio _)     = return val
eval env val@(Complex _)   = return val
eval env val@(Bool _)      = return val
-- variable
eval env (Atom var) = getVar env var
-- special forms
eval env (List (Atom "define" : args)) = defineForm env args
eval env (List (Atom "lambda" : args)) = lambdaForm env args
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) = F.load filename >>= evalBody env
eval env (List [Atom "if", pred, thenExp, elseExp]) = ifForm env pred thenExp elseExp
eval env (List (Atom "cond" : exps)) = condForm env exps
eval env (List (Atom "case" : pred : exps)) = caseForm env pred exps
-- function application
eval env (List (func : args)) = applyFunc env func args
-- or error
eval env badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


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


-- | apply function
applyFunc :: Env -> LispVal -> IOFunc
applyFunc env func args = do
  func <- eval env func
  vals <- mapM (eval env) args
  apply func vals


-- evaluate list of expressions and returns the value from last expression
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
evalBody env = liftM last . mapM (eval env)


-- ----------------------------------------------------------------------------------------
-- Special Form

-- helper to build function 
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map show params) varargs body env

makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarargsFunc :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarargsFunc = makeFunc . Just . show


defineForm :: Env -> [LispVal] -> IOThrowsError LispVal
-- variable
defineForm env [Atom var, form] = eval env form >>= defineVar env var
-- normal function: (define (hoge a b) ...)
defineForm env (List (Atom var : params) : body) =
  makeNormalFunc env params body >>= defineVar env var
-- varargs function: (define (hoge a . b) ...)
defineForm env (DottedList (Atom var : params) varargs : body) =
  makeVarargsFunc varargs env params body >>= defineVar env var
defineForm env badArgs = throwError $ SyntaxError "define" (List (Atom "define" : badArgs))


lambdaForm :: Env -> [LispVal] -> IOThrowsError LispVal
-- normal lambda expression: (lambda (a b) ...)
lambdaForm env (List params : body) = makeNormalFunc env params body
-- varargs lambda expression: (lambda (a . b) ...)
lambdaForm env (DottedList params varargs : body) = makeVarargsFunc varargs env params body
-- only varargs lambda expression: (lambda a ...)
lambdaForm env (varargs@(Atom _) : body) = makeVarargsFunc varargs env [] body
lambdaForm env badArgs = throwError $ SyntaxError "lambda" (List (Atom "lambda" : badArgs))


ifForm :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
ifForm env pred thenExp elseExp =
  do result <- eval env pred
     case result of
       Bool True  -> eval env thenExp
       Bool False -> eval env elseExp
       val        -> throwError $ TypeMismatchError "bool" val


condForm :: Env -> [LispVal] -> IOThrowsError LispVal
condForm env exps = case exps of
  []                    -> return Undefined
  List (pred:body) : xs -> do result <- eval env pred
                              case result of
                                Bool False -> condForm env xs
                                _          -> evalBody env body


caseForm :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
caseForm env pred exps =
  do base <- eval env pred
     case exps of
       []                            -> return Undefined
       List (List vs : body) : exps' -> do results <- liftThrowsError $ mapM (\v -> F.eqv [base, v]) vs
                                           Bool matched <- liftThrowsError $ or' results
                                           if matched
                                             then evalBody env body
                                             else caseForm env base exps'
       _                             -> throwError $ SyntaxError "case" (List (Atom "case" : pred : exps))
  where
    or' :: PrimitiveFunc
    or' [] = return (Bool False)
    or' (Bool False : xs) = or' xs
    or' (x:xs) = return x


-- ----------------------------------------------------------------------------------------
-- REPL

evalStringToLispVal :: Env -> String -> IO (ThrowsError LispVal)
evalStringToLispVal env expr = runErrorT $ do
  parsed <- liftThrowsError $ readExpr expr
  result <- eval env parsed
  return result

evalString :: Env -> String -> IO String
evalString env expr = do
  result <- evalStringToLispVal env expr
  return $ extractValue (fmap show result)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn 


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout


loadLibrary :: Env -> FilePath -> IO ()
loadLibrary env path = void . runIOThrowsError $
                      liftM show $ eval env $
                      List [Atom "load", String path]


initModule :: FilePath
initModule = "./lib/init.scm"

initEnv :: IO Env
initEnv = do
  env <- primitiveEnv
  loadLibrary env initModule
  return env

runOne :: [String] -> IO ()
runOne args = do
  env <- initEnv >>= bindVars `flip` [("args", List (map String $ drop 1 args))]
  runIOThrowsError (liftM show $ eval env $ List [Atom "load", String (head args)])
  >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do env <- initEnv
             catchIOError (loop env)  
              ( \e -> unless (isEOFError e) $ ioError e )
  where
    loop :: Env -> IO ()
    loop env = until_ (== "quit") (readPrompt "scheme> ") (evalAndPrint env)
