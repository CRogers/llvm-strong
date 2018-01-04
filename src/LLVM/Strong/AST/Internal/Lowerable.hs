module LLVM.Strong.AST.Internal.Lowerable (
    Lowerable(..),
) where

import qualified Data.Kind as Haskell (Type)

class Lowerable f where
    type Lower (f :: k -> Haskell.Type) :: Haskell.Type
    lower :: f a -> Lower f