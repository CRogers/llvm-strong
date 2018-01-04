module LLVM.Strong.AST.Internal.TypeIndexedList (
    Lowerable(..),
    TypeIndexedList,
) where

import qualified Data.Kind as Haskell (Type)

import LLVM.Strong.AST.Type (LlvmType(..))

class Lowerable f where
    type Lower (f :: k -> Haskell.Type) :: Haskell.Type
    lower :: f a -> Lower f

data TypeIndexedList :: (LlvmType -> Haskell.Type) -> [LlvmType] -> Haskell.Type where
    TILNil :: TypeIndexedList f '[]
    TILCons :: f a -> TypeIndexedList f as -> TypeIndexedList f (a ': as)

instance Lowerable f => Lowerable (TypeIndexedList f) where
    type Lower (TypeIndexedList f) = [Lower f]
    lower = lowerTypeIndexedList

lowerTypeIndexedList :: Lowerable f => TypeIndexedList f vals -> [Lower f]
lowerTypeIndexedList constants = case constants of
    TILNil -> []
    TILCons c cs -> lower c : lowerTypeIndexedList cs