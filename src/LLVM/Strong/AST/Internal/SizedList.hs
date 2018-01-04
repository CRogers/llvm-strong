module LLVM.Strong.AST.Internal.SizedList where

import GHC.TypeLits (Nat, type (+))
import qualified Data.Kind as Haskell (Type)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))

data SizedList :: Nat -> Haskell.Type -> Haskell.Type where
    SLNil :: SizedList 0 a
    SLCons :: a -> SizedList n a -> SizedList (n + 1) a

-- instanceLowerable SizedList 

lowerSizedList :: SizedList n a -> [a]
lowerSizedList list = case list of
    SLNil -> []
    SLCons a as -> a : lowerSizedList as