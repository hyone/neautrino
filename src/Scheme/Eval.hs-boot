module Scheme.Eval where

import Scheme.Type (LispVal, IOPrimitiveFunc)
import Scheme.Internal.Type (Env, IOThrowsError)

apply :: LispVal -> IOPrimitiveFunc
eval :: Env -> LispVal -> IOThrowsError LispVal
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
