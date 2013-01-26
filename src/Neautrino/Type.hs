-- | Neautrino Types
module Neautrino.Type
  ( SyntaxHandler
  , PrimitiveFunc
  , IOPrimitiveFunc
  , Closure(..)
  , LispVal(..)
  , ratio
  , complex
  , vector
  , isSymbol
  , isBool
  , isNumber
  , isString 
  , isList
  , isPair
  , isVector
  , isEnv
  , isAlias
  , isIdentifier
  , EvalExprMonad
  , runEvalExprMonad
  ) where

import Neautrino.Internal.Type
