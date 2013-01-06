-- | Internal DataType module.
module Scheme.Internal.Type
  ( SyntaxHandler
  , PrimitiveFunc
  , IOPrimitiveFunc
  , LispVal(..)
  , LispError(..)
  , ThrowsError
  , IOThrowsError
  , Env
  ) where

import Control.Monad.Error (Error(..), ErrorT)
import Data.Array (Array, elems)
import Data.Complex (Complex)
import Data.IORef (IORef)
import Text.Parsec (ParseError)
import System.IO (Handle)


-- Primitive Types -------------------------------------------------------

type SyntaxHandler = Env -> [LispVal] -> IOThrowsError LispVal

type PrimitiveFunc   = [LispVal] -> ThrowsError LispVal
type IOPrimitiveFunc = [LispVal] -> IOThrowsError LispVal

data LispVal = Atom String
             | List [LispVal]
             | Pair [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Integer Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool
             | Port Handle
             | Undefined
             | PrimitiveFunc PrimitiveFunc
             | IOPrimitiveFunc IOPrimitiveFunc
             | Syntax { syntaxName    :: String
                      , syntaxHandler :: SyntaxHandler }
             | Func { funcParams  :: [String]
                    , funcVararg  :: Maybe String
                    , funcBody    :: [LispVal]
                    , funcClosure :: Env }

-- Eq class instance

instance Eq LispVal where
  (Atom x)      == (Atom y)      = x == y 
  (Integer x)   == (Integer y)   = x == y 
  (Float x)     == (Float y)     = x == y 
  (Ratio x)     == (Ratio y)     = x == y 
  (Complex x)   == (Complex y)   = x == y 
  (Character x) == (Character y) = x == y 
  (String x)    == (String y)    = x == y 
  (Bool x)      == (Bool y)      = x == y 
  Undefined     == Undefined     = True
  List xs       == List ys       = xs == ys
  Pair xs x     == Pair ys y     = xs == ys && x == y
  Vector xs     == Vector ys     = xs == ys
  _             == _             = False

-- Show class instance

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Character c)      = "#\\" ++ [c]
showVal (String s)         = "\"" ++ s ++ "\""
showVal (Atom name)        = name
showVal (Integer i)        = show i
showVal (Float n)          = show n
showVal (Ratio n)          = show n
showVal (Complex n)        = show n
showVal (Bool True)        = "#t"
showVal (Bool False)       = "#f"
showVal (Port _)           = "#<io port>"
showVal Undefined          = "#<undef>"
showVal (Syntax name _)    = "#<syntax " ++ name ++ ">"
showVal PrimitiveFunc {}   = "#<primitive>"
showVal IOPrimitiveFunc {} = "#<io primitive>"
showVal Func {}            = "#<closure>"
showVal (List contents)    = "("  ++ unwordsList contents ++ ")"
showVal (Vector arr)       = "#(" ++ unwordsList (elems arr) ++ ")"
showVal (Pair h t)         = "("  ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList =  unwords . map showVal


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


-- Env Types -------------------------------------------------------

type Env = IORef [(String, IORef LispVal)]
