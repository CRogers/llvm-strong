{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PatternSynonyms #-}

module LLVM.Strong.AST.Instruction where

import Prelude hiding (Int)

import Data.Singletons (Apply)
import Data.Singletons.Prelude.List (SList, Sing(SCons, SNil), Map)
import Data.Singletons.TypeLits
import Data.Singletons.TH (singletons, genDefunSymbols)
import qualified GHC.TypeLits as TypeLits
import Data.Kind (Type)

$(singletons [d|

    -- See https://stackoverflow.com/a/36562893/139766
    data LlvmType_ nat
        = Void
        | Int_ nat
        | Pointer_ (LlvmType_ nat)
        | Function_ [LlvmType_ nat] (LlvmType_ nat)
        | Vector nat (LlvmType_ nat)
        | Struct_ [LlvmType_ nat]
        | Array nat (LlvmType_ nat)
        | Metadata
        | Label
        | Token
    |])

type LlvmType = LlvmType_ Nat 
type SLlvmType = (SLlvmType_ :: LlvmType -> Type)

type Int = (Int_ :: Nat -> LlvmType)
type I = Int

pattern SInt :: SNat i -> SLlvmType (Int i)
pattern SInt i = SInt_ i

type Pointer = (Pointer_ :: LlvmType -> LlvmType)

pattern SPointer :: SLlvmType t -> SLlvmType (Pointer t)
pattern SPointer t = SPointer_ t

type Function = (Function_ :: [LlvmType] -> LlvmType -> LlvmType)

pattern SFunction :: SList args -> SLlvmType ret -> SLlvmType (Function args ret)
pattern SFunction args ret = SFunction_ args ret

type Struct = (Struct_ :: [LlvmType] -> LlvmType)

pattern SStruct :: SList types -> SLlvmType (Struct types)
pattern SStruct types = SStruct_ types

type Name = String

type argTypes :-> retType = Function argTypes retType

pattern EmptyList = SNil
pattern List1 a = SCons a SNil
pattern List2 a b = a `SCons` List1 b
pattern List3 a b c = a `SCons` List2 b c
pattern List4 a b c d = a `SCons` List3 b c d

data Constant :: LlvmType -> Type where
    ConstantInt :: SLlvmType (Int i) -> Integer -> Constant (Int i)

data Operand :: LlvmType -> Type where
    LocalReference :: Name -> SLlvmType ty -> Operand ty
    ConstantOperand :: Constant ty -> Operand ty

struct :: SLlvmType (Struct '[
        '[ Int 8 ]          :-> Pointer (Int 32),
        '[ Pointer 'Void ] :-> Int 8
    ] )
struct = SStruct $ List2
    (SFunction (List1 i8) (SPointer i32))
    (SFunction (List1 $ SPointer SVoid) i8)

function :: SLlvmType ('[Int 32] :-> (Int 8))
function = SFunction (List1 i32) i8

refToI8 :: Operand (Int 8)
refToI8 = LocalReference "s" i8

voidType :: SLlvmType 'Void
voidType = SVoid

intType :: KnownNat n => SLlvmType (Int n)
intType = SInt SNat

i1 :: SLlvmType (Int 1)
i1 = intType @1

i8 :: SLlvmType (Int 8)
i8 = intType @8

i32 :: SLlvmType (Int 32)
i32 = intType @32

pointerToI8 :: SLlvmType (Pointer (Int 8))
pointerToI8 = SPointer i8

pointerToPointerToPointerToI8 :: SLlvmType (Pointer (Pointer (Pointer (Int 8))))
pointerToPointerToPointerToI8 = SPointer (SPointer (SPointer i8))
