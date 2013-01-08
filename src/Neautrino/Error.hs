module Neautrino.Error
  ( LispError(..)
  , ThrowsError
  , IOThrowsError
  , extractValue
  , liftThrowsError
  -- * Convenience re-exports
  , ParseError
  , catchError
  , runErrorT
  , throwError
  ) where

import Neautrino.Internal.Type ( LispError(..), ThrowsError, IOThrowsError
                               , extractValue, liftThrowsError )
import Control.Monad.Error (catchError, throwError, runErrorT)
import Text.Parsec (ParseError)
