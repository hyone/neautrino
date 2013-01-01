module Scheme.Env
  ( Env
  , getVar
  , setVar
  , defineVar
  , bindVars
  , primitiveEnv
  ) where

import Scheme.Error
import Scheme.Type
import Scheme.Function (primitives, ioPrimitives)

import Data.IORef
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)


type Var = String
type VarPair = (Var, LispVal)
type VarRefPair = (Var, IORef LispVal)
type Env = IORef [VarRefPair]


nullEnv :: IO Env
nullEnv = newIORef []

-- | whether or not the variable is bounded
isBound :: Env -> Var -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

-- | get value of variable
getVar :: Env -> Var -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

-- | set value of variable
setVar :: Env -> Var -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

-- | modify value of an existing variable or create new one
defineVar :: Env -> Var -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  defined <- liftIO $ isBound envRef var
  if defined
     then setVar envRef var value
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
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)

    addBinding :: VarPair -> IO VarRefPair
    addBinding (var, value) = liftM (\ref -> (var, ref)) $ newIORef value


primitiveEnv :: IO Env
primitiveEnv = nullEnv >>=
                (flip bindVars $
                  map (buildFunc IOPrimitiveFunc) ioPrimitives ++
                  map (buildFunc PrimitiveFunc) primitives)
  where
    buildFunc :: (a -> b) -> (c, a) -> (c, b)
    buildFunc constructor (var, func) = (var, constructor func)

