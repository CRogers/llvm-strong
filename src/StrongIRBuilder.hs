{-# LANGUAGE RebindableSyntax #-}

module StrongIRBuilder where

import Prelude hiding ((>>), (>>=), return)

import Control.Monad.Indexed (IxMonad(..), IxPointed(..), (>>>=), ireturn)

return :: IxPointed m => a -> m i i a
return = ireturn

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

data IRBuilderState = Middle | Terminator

data StrongIRBuilder s = StrongIRBuilder

