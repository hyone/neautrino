-- | Scheme Types
module Scheme.Type
  ( PrimitiveFunc
  , IOFunc
  , LispVal(..)
  ) where

import Scheme.Internal.Type

import Data.Array (elems)


-- Eq class instance

instance Eq LispVal where
  (Atom x)        == (Atom y)        = x == y 
  (Number x)      == (Number y)      = x == y 
  (Float x)       == (Float y)       = x == y 
  (Ratio x)       == (Ratio y)       = x == y 
  (Complex x)     == (Complex y)     = x == y 
  (Character x)   == (Character y)   = x == y 
  (String x)      == (String y)      = x == y 
  (Bool x)        == (Bool y)        = x == y 
  Undefined       == Undefined       = True
  List xs         == List ys         = xs == ys
  DottedList xs x == DottedList ys y = xs == ys && x == y
  Vector xs       == Vector ys       = xs == ys
  _               == _               = False


-- Show class instance

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Character c)      = "#\\" ++ [c]
showVal (String s)         = "\"" ++ s ++ "\""
showVal (Atom name)        = name
showVal (Number i)         = show i
showVal (Float n)          = show n
showVal (Ratio n)          = show n
showVal (Complex n)        = show n
showVal (Bool True)        = "#t"
showVal (Bool False)       = "#f"
showVal (Port _)           = "#<io port>"
showVal Undefined          = "#<undef>"
showVal PrimitiveFunc {}   = "#<primitive>"
showVal IOPrimitiveFunc {} = "#<io primitive>"
showVal Func {}            = "#<closure>"
showVal (List contents)    = "("  ++ unwordsList contents ++ ")"
showVal (Vector arr)       = "#(" ++ unwordsList (elems arr) ++ ")"
showVal (DottedList h t)   = "("  ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList =  unwords . map showVal
