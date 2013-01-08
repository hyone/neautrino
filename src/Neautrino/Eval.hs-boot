module Neautrino.Eval where

import Neautrino.Type (LispVal, IOPrimitiveFunc)
import Neautrino.Internal.Type (Env, IOThrowsError)

apply :: LispVal -> IOPrimitiveFunc
eval :: Env -> LispVal -> IOThrowsError LispVal
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
