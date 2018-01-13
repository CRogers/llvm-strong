{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module LLVM.Strong.AST.Internal.TypeIndexedList (
    TypeIndexedList,
    list0, list1
) where

import qualified Data.Kind as Haskell (Type)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))

data TypeIndexedList :: (k -> Haskell.Type) -> [k] -> Haskell.Type where
    TILNil :: TypeIndexedList f '[]
    TILCons :: f a -> TypeIndexedList f as -> TypeIndexedList f (a ': as)

instance Lowerable f => Lowerable (TypeIndexedList f) where
    type Lower (TypeIndexedList f) = [Lower f]
    lower constants = case constants of
        TILNil -> []
        TILCons c cs -> lower c : lower cs

list0 :: TypeIndexedList f '[]
list0 = TILNil

list1 :: f a -> TypeIndexedList f '[a]
list1 a = a `TILCons` list0

list2 :: f a -> f b -> TypeIndexedList f '[a, b]
list2 a b = a `TILCons` list1 b