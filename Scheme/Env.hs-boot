module Scheme.Env where

import Data.IORef
import {-# SOURCE #-} Scheme.Type

type Env = IORef [(String, IORef LispVal)]
