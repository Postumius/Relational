module Utilities
( (.-)
, whenM
, explain
) where

import Control.Monad.State

infixr 8 .-
(.-) f g = \x -> g (f x)

whenM :: Monad m => m Bool -> m () -> m ()
whenM boolM action = do 
  bool <- boolM
  when bool action
  
explain err Nothing = Left err
explain err (Just val) = Right val
