module Neautrino.Eval
  ( apply
  , applyMacroTransformer
  , eval
  , evalAST
  , evalBody
  , evalString
  ) where

import Neautrino.Type
import Neautrino.Env
import Neautrino.Error
import Neautrino.Parser (readExpr)

import Control.Arrow (first)
import Control.Monad (liftM, forM_)
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
      Syntax _ handler -> handler args
      MacroTransformer _ mproc -> do
        useEnv        <- ask
        macroexpanded <- lift $ applyMacroTransformer mproc (List (app:args)) useEnv
        -- and eval once
        eval macroexpanded
      -- procedure
      _ -> do vals <- mapM eval args
              lift $ apply f vals


applyClosure :: Closure -> [LispVal] -> IOErrorM LispVal
applyClosure (Closure' params varargs body closEnv) args =
    if length params /= length args && isNothing varargs then
      throwError $ NumArgsError (length params) args
    else do
      let argPairs   = zip params args
          atomPairs  = filter (isSymbol . fst) argPairs
          aliasPairs = filter (isAlias  . fst) argPairs
      -- add alias argument bindings to each syntactic environment
      liftIO $ forM_ aliasPairs $ \(SyntacticClosure synEnv _ (Atom var), value) ->
        pushVar synEnv var value
      -- bind normal arguments
      env  <- liftIO (bindVars closEnv (map (first atomName) atomPairs))
                >>= bindVarArgs varargs
      -- eval body
      ret <- runReaderT (evalBody body) env
      -- remove alias argument bindings to each syntactic environment
      liftIO $ forM_ aliasPairs $ \(SyntacticClosure synEnv _ _, _) ->
        popVar synEnv
      return ret
  where
    bindVarArgs :: Maybe String -> Env -> IOErrorM Env
    bindVarArgs varg env' = case varg of
      Just argName -> liftIO $ bindVars env' [(argName, List remainingArgs)]
      Nothing      -> return env'
    remainingArgs :: [LispVal]
    remainingArgs = drop (length params) args


applyMacroTransformer :: Closure -> LispVal -> Env -> IOErrorM LispVal
applyMacroTransformer mproc@(Closure' _ _ _ macEnv) expr useEnv =
  applyClosure mproc [expr, SyntacticEnv useEnv, SyntacticEnv macEnv]


-- | apply function to argument list
apply :: LispVal -> [LispVal] -> IOErrorM LispVal
apply (PrimitiveFunc   func) args = liftErrorM (func args)
apply (IOPrimitiveFunc func) args = func args
apply (Closure closure)      args = applyClosure closure args
apply notFunc _ = throwError $ NotFunctionError "invalid application" (show notFunc)


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
