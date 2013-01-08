module Neautrino.Eval where

import Neautrino.Internal.Type (LispVal, EvalExprMonad, IOPrimitiveFunc)

apply :: LispVal -> IOPrimitiveFunc
eval  :: LispVal -> EvalExprMonad LispVal
evalBody :: [LispVal] -> EvalExprMonad LispVal
