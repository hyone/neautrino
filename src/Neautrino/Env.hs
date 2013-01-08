module Neautrino.Env
  ( Env
  , Var
  , VarPair
  , VarRefPair
  , nullEnv
  , getVar
  , setVar
  , defineVar
  , bindVars
  ) where

import Neautrino.Error
import Neautrino.Internal.Type (Env, EvalExprMonad, LispVal)

import Data.IORef
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Maybe (isJust)


type Var = String
type VarPair = (Var, LispVal)
type VarRefPair = (Var, IORef LispVal)


nullEnv :: IO Env
nullEnv = newIORef []

-- | whether or not the variable is bounded
isBound :: Env -> Var -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

-- | get value of variable
getVar :: Var -> EvalExprMonad LispVal
getVar var = do
  envRef <- ask
  env    <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

-- | set value of variable
setVar :: Var -> LispVal -> EvalExprMonad LispVal
setVar var value = do
  envRef <- ask
  env    <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

-- | modify value of an existing variable or create new one
defineVar :: Var -> LispVal -> EvalExprMonad LispVal
defineVar var value = do
  envRef  <- ask
  defined <- liftIO $ isBound envRef var
  if defined
     then setVar var value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value


-- | extend env with bindings
bindVars :: Env -> [VarPair] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv :: [VarPair] -> [VarRefPair] -> IO [VarRefPair]
    extendEnv bindings' env = liftM (++ env) (mapM addBinding bindings')

    addBinding :: VarPair -> IO VarRefPair
    addBinding (var, value) = liftM (\ref -> (var, ref)) $ newIORef value