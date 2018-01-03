{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module LLVM.Strong.AST.Instruction where

import Data.Singletons (SingI, Sing)
import Data.Singletons.TypeLits
import Data.Singletons.TH (singletons)
import qualified GHC.TypeLits as TypeLits
import Data.Kind (Type)

$(singletons [d|

    -- See https://stackoverflow.com/a/36562893/139766
    data LlvmType_ nat
        = Void
        | Integer nat
        | Pointer (LlvmType_ nat)
        | Function (LlvmType_ nat) [LlvmType_ nat]

    data Test = Foo [Bool]
    |])

type LlvmType = LlvmType_ Nat
type SLlvmType = SLlvmType_

type Name = String

data Operand :: LlvmType -> Type where
    LocalReference :: Name -> SLlvmType ty -> Operand ty

refToI8 :: Operand ('Integer 8)
refToI8 = LocalReference "s" i8

voidType :: SLlvmType 'Void
voidType = SVoid

i1 :: SLlvmType ('Integer 1)
i1 = SInteger (SNat @1)

i8 :: SLlvmType ('Integer 8)
i8 = SInteger (SNat @8)

pointerToI8 :: SLlvmType ('Pointer ('Integer 8))
pointerToI8 = SPointer i8

pointerToPointerToPointerToI8 :: SLlvmType ('Pointer ('Pointer ('Pointer ('Integer 8))))
pointerToPointerToPointerToI8 = SPointer (SPointer (SPointer i8))
