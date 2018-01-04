{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PatternSynonyms #-}

module LLVM.Strong.AST.NoSingletons (LlvmType(..), Type, Operand, Constant) where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))

import Data.Singletons (fromSing)
import Data.Singletons.Prelude.List (SList, Sing(SCons, SNil))

import qualified LLVM.AST.AddrSpace as LLVM (AddrSpace(..))
import qualified LLVM.AST.Type as LLVM (Type(..))
import qualified LLVM.AST.Operand as LLVM (Operand(..))
import qualified LLVM.AST.Constant as LLVM (Constant(..))

data LlvmType
    = Void
    | Int Nat
    | Pointer LlvmType
    | Function [LlvmType] LlvmType

newtype Type (ty :: LlvmType) = Type { llvmType :: LLVM.Type }
    deriving (Show, Eq)

voidType :: Type Void
voidType = Type LLVM.VoidType

integerType :: forall n. KnownNat n => Type (Int n)
integerType = Type . LLVM.IntegerType . fromInteger $ natVal (Proxy :: Proxy n)

i1 :: Type (Int 1)
i1 = integerType @1

i8 :: Type (Int 8)
i8 = integerType @8

i32 :: Type (Int 32)
i32 = integerType @32

pointerTo :: Type ty -> Type (Pointer ty)
pointerTo (Type ty) = Type (LLVM.PointerType ty (LLVM.AddrSpace 0))

functionType :: SList args -> Type (Function args Void)
functionType args = _ $ fromSing args

newtype Operand (ty :: LlvmType) = Operand LLVM.Operand
newtype Constant (ty :: LlvmType) = Constant LLVM.Constant

