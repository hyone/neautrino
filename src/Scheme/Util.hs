module Scheme.Util (
  until_
) where

import Control.Monad (unless)


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ do
    action result
    until_ pred prompt action
