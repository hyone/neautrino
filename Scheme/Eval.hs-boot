module Scheme.Eval where

import Scheme.Type (LispVal, IOFunc)
import Scheme.Internal.Type (Env, IOThrowsError)

apply :: LispVal -> IOFunc
eval :: Env -> LispVal -> IOThrowsError LispVal
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
