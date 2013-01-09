-- | This module simply re-exports from other modules for your convenience.
module Neautrino
  ( eval
  , evalAST
  , evalString
  , initEnv
  , scheme
  ) where

import Neautrino.TH (scheme)
import Neautrino.Eval (eval, evalString, evalAST)
import Neautrino.Run (initEnv)
