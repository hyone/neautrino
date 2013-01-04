-- | Internal DataType module.
module Scheme.Internal.Type
  ( PrimitiveFunc
  , IOFunc
  , LispVal(..)
  , LispError(..)
  , ThrowsError
  , IOThrowsError
  , Env
  ) where

import Control.Monad.Error (ErrorT)
import Data.Array (Array)
import Data.Complex (Complex)
import Data.IORef (IORef)
import Text.Parsec (ParseError)
import System.IO (Handle)


-- Primitive Types -------------------------------------------------------

type PrimitiveFunc = [LispVal] -> ThrowsError LispVal
type IOFunc = [LispVal] -> IOThrowsError LispVal

data LispVal = Atom String
             | List [LispVal]
             | Pair [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool
             | Port Handle
             | Undefined
             | PrimitiveFunc PrimitiveFunc
             | IOPrimitiveFunc IOFunc
             | Func { fParams :: [String],
                      fVararg :: Maybe String,
                      fBody :: [LispVal],
                      fClosure :: Env }


-- Error Types -------------------------------------------------------

data LispError = NumArgsError Int [LispVal]
               | TypeMismatchError String LispVal
               | ParserError ParseError
               | SyntaxError String LispVal
               | BadSpecialFormError String LispVal
               | NotFunctionError String String
               | UnboundVarError String String
               | DefaultError String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO


-- Env Types -------------------------------------------------------

type Env = IORef [(String, IORef LispVal)]
