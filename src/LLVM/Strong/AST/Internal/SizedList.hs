module LLVM.Strong.AST.Internal.SizedList (
    SizedList,
) where

import GHC.TypeLits (Nat, type (+))
import qualified Data.Kind as Haskell (Type)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))

data SizedList :: Haskell.Type -> Nat -> Haskell.Type where
    SLNil :: SizedList a 0
    SLCons :: a -> SizedList a n -> SizedList a (n + 1)

instance Lowerable (SizedList a) where
    type Lower (SizedList a) = [a]
    lower = lowerSizedList

lowerSizedList :: SizedList a n -> [a]
lowerSizedList list = case list of
    SLNil -> []
    SLCons a as -> a : lowerSizedList as