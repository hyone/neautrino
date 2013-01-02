module Scheme.Util (until_) where

import Control.Monad (unless)


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  unless (p result) $ do
    action result
    until_ p prompt action
