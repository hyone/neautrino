module Scheme.Error (
  LispError(..)
, ThrowsError
, IOThrowsError
, trapError
, extractValue
, liftThrowsError
, runIOThrowsError
-- * Convenience re-exports
, throwError
) where

import Scheme.Type (LispVal)

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)


data LispError = NumArgsError Int [LispVal]
               | TypeMismatchError String LispVal
               | ParserError ParseError
               | SyntaxError String LispVal
               | BadSpecialFormError String LispVal
               | NotFunctionError String String
               | UnboundVarError String String
               | DefaultError String


instance Error LispError where
  noMsg  = DefaultError "An error has occurred"
  strMsg = DefaultError


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


type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

{-
   We purposely leave extractValue undefined for a Left constructor,
   because that represents a programmer error.
   We intend to use extractValue only after a catchError
-}
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


type IOThrowsError = ErrorT LispError IO

liftThrowsError :: ThrowsError a -> IOThrowsError a
liftThrowsError (Left err)  = throwError err
liftThrowsError (Right val) = return val

runIOThrowsError :: IOThrowsError String -> IO String
runIOThrowsError action = liftM extractValue $ runErrorT (trapError action)
