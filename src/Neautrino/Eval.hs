module Neautrino.Eval
  ( apply
  , eval
  , evalAST
  , evalBody
  , evalString
  ) where

import Neautrino.Type (LispVal(..), EvalExprMonad, runEvalExprMonad)
import Neautrino.Env (Env, Var, bindVars, getVar, bindFreevars)
import Neautrino.Error
import Neautrino.Parser (readExpr)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ask, local, runReaderT)
import Data.Maybe (isNothing)


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
eval Undefined         = return Undefined
-- variable
eval (Atom var) = getVar var
-- syntactic closure
eval (SyntacticClosure env freevars expr) = evalSyntacticClosure env freevars expr
-- special form, macro and procedure application
eval (List (app : args)) = evalApplication app args
-- otherwise error
eval badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


evalSyntacticClosure :: Env -> [Var] -> LispVal -> EvalExprMonad LispVal
evalSyntacticClosure synEnv freevars expr = do
  useEnv <- ask
  env    <- liftIO $ bindFreevars freevars useEnv synEnv
  local (const env) $ eval expr


evalApplication :: LispVal -> [LispVal] -> EvalExprMonad LispVal
evalApplication app args = do
    f <- eval app
    case f of
      (Syntax _ handler) -> handler args
      (Macro _ params varargs transformer env) -> do
        -- expand macro
        expandedExp <- lift $ applyClosure params varargs transformer env args
        -- and eval once
        eval expandedExp
      -- procedure
      _ -> do vals <- mapM eval args
              lift $ apply f vals

applyClosure :: [String]
             -> Maybe String
             -> [LispVal]
             -> Env
             -> [LispVal]
             -> IOErrorM LispVal
applyClosure params varargs body env args =
    if length params /= length args && isNothing varargs then
      throwError $ NumArgsError (length params) args
    else
      liftIO (bindVars env (zip params args))
        >>= bindVarArgs varargs
        >>= runReaderT (evalBody body)
  where
    bindVarArgs :: Maybe String -> Env -> IOErrorM Env
    bindVarArgs varg env' = case varg of
      Just argName -> liftIO $ bindVars env' [(argName, List remainingArgs)]
      Nothing      -> return env'
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

-- | apply function to argument list
apply :: LispVal -> [LispVal] -> IOErrorM LispVal
apply (PrimitiveFunc   func)         args = liftErrorM (func args)
apply (IOPrimitiveFunc func)         args = func args
apply (Func params varargs body env) args = applyClosure params varargs body env args
apply notFunc                        _    =
  throwError $ NotFunctionError "invalid application" (show notFunc)


-- | evaluate list of expressions and returns the value from last expression
evalBody :: [LispVal] -> EvalExprMonad LispVal
evalBody = liftM last . mapM eval


-- | evaluate abstract syntax tree (LispVal) and returns its result
evalAST :: Env -> LispVal -> IO (ErrorM LispVal)
evalAST env = runEvalExprMonad env . eval


evalStringAST :: Env -> String -> IO (ErrorM LispVal)
evalStringAST env expr = runEvalExprMonad env $ do
  parsed <- liftErrorM $ readExpr expr
  eval parsed

-- | eval String and return its result as String
evalString :: Env -> String -> IO String
evalString env expr = do
  result <- evalStringAST env expr
  return $ extractValue result
