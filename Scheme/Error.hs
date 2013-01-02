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
import Scheme.Type ()

import Control.Monad.Error
import Text.Parsec (ParseError)


-- Error class instance

instance Error LispError where
  noMsg  = DefaultError "An error has occurred"
  strMsg = DefaultError


-- Show class instance
 
instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVarError message varname) = message ++ ": " ++ varname
showError (BadSpecialFormError message form) = message ++ ": " ++ show form
showError (NotFunctionError message func) = message ++ ": " ++ show func
showError (SyntaxError message exps) = "Syntax error at " ++ message ++ ": " ++ show exps
showError (NumArgsError expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ (unwords . map show) found
showError (TypeMismatchError expected found) = "Invalid type: expected " ++ show expected
                                            ++ ", found " ++ show found
showError (ParserError parseError) = "Parse error at " ++ show parseError
showError (DefaultError message) = "Error: " ++ message


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
