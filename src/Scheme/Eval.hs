module Scheme.Eval (
  apply,
  eval,
  evalString,
  runOne,
  runRepl
) where

import Scheme.Type
import Scheme.Env
import Scheme.Error
import Scheme.Parser (readExpr, readExprList)
import qualified Scheme.Function as F

import Control.Monad
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import Text.ParserCombinators.Parsec (parse)
import System.IO (hFlush, hPutStrLn, stderr, stdout)


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
-- error
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


-- function application
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
makeFunc :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map show params) varargs body env

makeNormalFunc :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarargsFunc :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarargsFunc = makeFunc . Just . show


defineForm :: Env -> [LispVal] -> IOThrowsError LispVal
-- variable
defineForm env [Atom var, form] = eval env form >>= defineVar env var
-- normal function
defineForm env (List (Atom var : params) : body) =
  makeNormalFunc env params body >>= defineVar env var
-- varargs function
defineForm env (DottedList (Atom var : params) varargs : body) =
  makeVarargsFunc varargs env params body >>= defineVar env var
defineForm env badArgs = throwError $ SyntaxError "define" (List (Atom "define" : badArgs))


lambdaForm :: Env -> [LispVal] -> IOThrowsError LispVal
-- normal lambda expression
lambdaForm env (List params : body) = makeNormalFunc env params body
-- varargs lambda expression
lambdaForm env (DottedList params varargs : body) = makeVarargsFunc varargs env params body
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

evalString :: Env -> String -> IO String
-- evalString env expr = runIOThrowsError . liftM show $
--                         liftThrowsError (readExpr expr) >>= eval env
evalString env expr = runIOThrowsError $ do
  parsed <- liftThrowsError $ readExpr expr
  result <- eval env parsed
  return $ show result


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn 


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $
    action result >> until_ pred prompt action

loadLibraries :: Env -> IO ()
loadLibraries env = void . runIOThrowsError $
                      liftM show $ eval env $
                      List [Atom "load", String "/Users/hiro/program/haskell/scheme/lib/init.scm"]

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveEnv >>= bindVars `flip` [("args", List (map String $ drop 1 args))]
  runIOThrowsError (liftM show $ eval env $ List [Atom "load", String (head args)])
  >>= hPutStrLn stderr


runRepl :: IO ()
runRepl = primitiveEnv
          >>= until_ (== "quit") (readPrompt "lisp> ") . evalAndPrint
