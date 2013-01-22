{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Neautrino.Env
  ( Env
  , Var
  , VarPair
  , VarRefPair
  , nullEnv
  , isBound
  , getVar
  , setVar
  , unsetVar
  , defineVar
  , bindVars
  , bindFreevars
  , equalVar
  , dumpEnv
  ) where

import Neautrino.Error
import Neautrino.Internal.Type (Env, Var, EvalExprMonad, LispVal(..))

import Data.IORef
import Control.Monad (forM, liftM, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Maybe (fromJust, isJust)

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

-- | set value to variable in env
setVar :: Var -> LispVal -> EvalExprMonad LispVal
setVar var value = do
  envRef <- ask
  env    <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVarError "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

-- | unset name from env
unsetVar :: Var -> EvalExprMonad ()
unsetVar var = do
  envRef <- ask
  env    <- liftIO $ readIORef envRef
  liftIO $ writeIORef envRef (filter ((/= var) . fst) env)
  return ()

addVar :: Env -> Var -> LispVal -> IO LispVal
addVar envRef var value = do
  env      <- readIORef envRef
  valueRef <- newIORef value
  writeIORef envRef ((var, valueRef) : env)
  return value

-- | modify value of an existing variable or create new one
defineVar :: Var -> LispVal -> EvalExprMonad LispVal
defineVar var value = do
  envRef  <- ask
  defined <- liftIO $ isBound envRef var
  if defined then
    setVar var value
  else
    liftIO $ addVar envRef var value
  return value


-- | extend env with bindings
bindVars :: Env -> [VarPair] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv :: [VarPair] -> [VarRefPair] -> IO [VarRefPair]
    extendEnv bindings' env = liftM (++ env) (mapM addBinding bindings')

    addBinding :: VarPair -> IO VarRefPair
    addBinding (var, value) = liftM (\ref -> (var, ref)) $ newIORef value


equalVar :: Env -> Var -> Env -> Var -> IO Bool
equalVar envRef1 var1 envRef2 var2 = do
  env1 <- readIORef envRef1
  env2 <- readIORef envRef2
  let field1 = lookup var1 env1
  let field2 = lookup var2 env2
  return $ case (field1, field2) of
    (Just ref1, Just ref2) -> ref1 == ref2
    _                      -> False

bindFreevars :: [Var] -> Env -> Env -> IO Env
bindFreevars freevars fromEnvRef toEnvRef = do
  toEnv  <- readIORef toEnvRef
  adding <- forM freevars $ \varname -> do
    defined <- isBound fromEnvRef varname
    unless defined $
      void (addVar fromEnvRef varname Undefined)
    valueRef <- (fromJust . lookup varname) `liftM` readIORef fromEnvRef
    return (varname, valueRef)
  newIORef (adding ++ toEnv)

dumpEnv :: Env -> IO [VarPair]
dumpEnv envRef = do
  env <- readIORef envRef
  forM (map fst env) $ \n -> do
    v <- readIORef $ fromJust $ lookup n env
    return (n, v)
