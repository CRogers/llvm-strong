{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module LLVM.Strong.AST.Instruction where

import Data.Singletons (Apply)
import Data.Singletons.Prelude.List (Sing(SCons, SNil), Map)
import Data.Singletons.TypeLits
import Data.Singletons.TH (singletons, genDefunSymbols)
import qualified GHC.TypeLits as TypeLits
import Data.Kind (Type)

$(singletons [d|

    -- See https://stackoverflow.com/a/36562893/139766
    data LlvmType_ nat
        = Void
        | Integer nat
        | Pointer (LlvmType_ nat)
        | Function (LlvmType_ nat) [LlvmType_ nat] 

    data LLVMType
        = IntegerType
        | FloatType
        | FunctionType [LLVMType]
    |])

type LlvmType = LlvmType_ Nat 
type SLlvmType = SLlvmType_

type Name = String

data Constant :: LlvmType -> Type where
    Int :: SLlvmType ('Integer i) -> Integer -> Constant ('Integer i)

data Operand :: LlvmType -> Type where
    LocalReference :: Name -> SLlvmType ty -> Operand ty
    ConstantOperand :: Constant ty -> Operand ty

type family ToOperand (ty :: LlvmType) = b | b -> ty where
    ToOperand ty = Operand ty

$(genDefunSymbols [''ToOperand])

function :: SLlvmType ('Function ('Integer 8) '[ 'Integer 32])
function = SFunction i8 (SCons i32 SNil)

refToI8 :: Operand ('Integer 8)
refToI8 = LocalReference "s" i8

voidType :: SLlvmType 'Void
voidType = SVoid

intType :: KnownNat n => SLlvmType ('Integer n)
intType = SInteger SNat

i1 :: SLlvmType ('Integer 1)
i1 = intType @1

i8 :: SLlvmType ('Integer 8)
i8 = intType @8

i32 :: SLlvmType ('Integer 32)
i32 = intType @32

pointerToI8 :: SLlvmType ('Pointer ('Integer 8))
pointerToI8 = SPointer i8

pointerToPointerToPointerToI8 :: SLlvmType ('Pointer ('Pointer ('Pointer ('Integer 8))))
pointerToPointerToPointerToI8 = SPointer (SPointer (SPointer i8))
