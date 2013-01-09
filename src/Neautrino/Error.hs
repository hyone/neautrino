module Neautrino.Error
  ( LispError(..)
  , ErrorM
  , IOErrorM
  , extractValue
  , liftErrorM
  -- * Convenience re-exports
  , ParseError
  , catchError
  , runErrorT
  , throwError
  ) where

import Neautrino.Internal.Type ( LispError(..), ErrorM, IOErrorM
                               , extractValue, liftErrorM )
import Control.Monad.Error (catchError, throwError, runErrorT)
import Text.Parsec (ParseError)
