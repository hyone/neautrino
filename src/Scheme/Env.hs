module Scheme.Env (
  Env,
  getVar,
  setVar,
  defineVar,
  bindVars,
  primitiveEnv
) where

import Scheme.Error
import Scheme.Type
import Scheme.Function (primitives, ioPrimitives)

import Data.IORef
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)


type Env = IORef [(String, IORef LispVal)]


nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  defined <- liftIO $ isBound envRef var
  if defined
    then setVar envRef var value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv :: [(String, LispVal)] -> [(String, IORef LispVal)] -> IO [(String, IORef LispVal)]
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)

    addBinding :: (String, LispVal) -> IO (String, IORef LispVal)
    addBinding (var, value) = liftM (\ref -> (var, ref)) $ newIORef value


primitiveEnv :: IO Env
primitiveEnv = nullEnv >>=
                 (flip bindVars $ 
                    map (buildFunc IOPrimitiveFunc) ioPrimitives ++
                    map (buildFunc PrimitiveFunc) primitives)
  where
    buildFunc constructor (var, func) = (var, constructor func)
