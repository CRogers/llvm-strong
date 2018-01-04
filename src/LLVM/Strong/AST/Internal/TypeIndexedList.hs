module LLVM.Strong.AST.Internal.TypeIndexedList (
    TypeIndexedList,
) where

import qualified Data.Kind as Haskell (Type)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))
import LLVM.Strong.AST.Type (LlvmType(..))

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