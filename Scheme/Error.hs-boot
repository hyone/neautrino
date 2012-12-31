module Scheme.Error where

import Control.Monad.Error (ErrorT)


data LispError

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO
