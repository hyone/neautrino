module Neautrino.Eval
  ( apply
  , eval
  , evalBody
  , evalString
  ) where

import Neautrino.Type (LispVal(..), EvalExprMonad, runEvalExprMonad)
import Neautrino.Env (Env, bindVars, getVar)
import Neautrino.Error
import Neautrino.Parser (readExpr)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (runReaderT)
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
-- variable
eval (Atom var) = getVar var
-- special form or function application
eval (List (procedure : args)) = applyProcedure procedure args
-- or error
eval badForm = throwError $ BadSpecialFormError "Unrecognized special form" badForm


-- | apply function to argument list
apply :: LispVal -> [LispVal] -> IOErrorM LispVal
apply (PrimitiveFunc   func) args = liftErrorM (func args)
apply (IOPrimitiveFunc func) args = func args
apply (Func params varargs body env) args =
    if length params /= length args && isNothing varargs then
      throwError $ NumArgsError (length params) args
    else
      liftIO (bindVars env (zip params args))
        >>= bindVarArgs varargs
        >>= runReaderT (evalBody body)
  where
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args

    bindVarArgs :: Maybe String -> Env -> IOErrorM Env
    bindVarArgs arg env' = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env'
apply notFunc _ = throwError $ NotFunctionError "invalid application" (show notFunc)


applyProcedure :: LispVal -> [LispVal] -> EvalExprMonad LispVal
applyProcedure procedure args = do
    f <- eval procedure
    case f of
      (Syntax _ handler) -> handler args
      _                  -> do vals <- mapM eval args
                               lift $ apply f vals


-- | evaluate list of expressions and returns the value from last expression
evalBody :: [LispVal] -> EvalExprMonad LispVal
evalBody = liftM last . mapM eval


evalStringAST :: Env -> String -> IO (ErrorM LispVal)
evalStringAST env expr = runEvalExprMonad env $ do
  parsed <- liftErrorM $ readExpr expr
  eval parsed

-- | eval String and return its result as String
evalString :: Env -> String -> IO String
evalString env expr = do
  result <- evalStringAST env expr
  return $ extractValue result
