module Scheme.Eval where

import Scheme.Type  (LispVal, IOFunc)
import Scheme.Error (IOThrowsError)

apply :: LispVal -> IOFunc
