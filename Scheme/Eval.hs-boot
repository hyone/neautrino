module Scheme.Eval where

import Scheme.Type (LispVal, IOFunc)
import Scheme.Internal.Type (Env, IOThrowsError)

apply :: LispVal -> IOFunc

evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
