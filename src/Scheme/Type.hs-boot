module Scheme.Type where

import {-# SOURCE #-} Scheme.Error


type PrimitiveFunc = [LispVal] -> ThrowsError LispVal
type IOFunc = [LispVal] -> IOThrowsError LispVal

data LispVal
