module Scheme.Error
  ( LispError(..)
  , ThrowsError
  , IOThrowsError
  , extractValue
  , liftThrowsError
  , runIOThrowsError
  -- * Convenience re-exports
  , ParseError
  , throwError
  ) where

import Scheme.Internal.Type (LispError(..), ThrowsError, IOThrowsError)

import Control.Monad (liftM)
import Control.Monad.Error (throwError, runErrorT)
import Text.Parsec (ParseError)


-- Functions

-- | extractValue from ThrowsError to String
extractValue :: ThrowsError String -> String
extractValue (Right val) = val
extractValue (Left err)  = show err

-- | lift ThrowsError to IOThrowsError.
liftThrowsError :: ThrowsError a -> IOThrowsError a
liftThrowsError (Left err)  = throwError err
liftThrowsError (Right val) = return val

-- | run IOThrowsError.
runIOThrowsError :: IOThrowsError String -> IO String
runIOThrowsError = liftM extractValue . runErrorT
