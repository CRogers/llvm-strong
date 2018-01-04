{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PatternSynonyms #-}

module LLVM.Strong.AST.NoSingletons (LlvmType(..), Type, Operand, Constant) where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))
import Data.Coerce (coerce)

import Data.Word (Word32)

import qualified LLVM.AST.AddrSpace as LLVM (AddrSpace(..))
import qualified LLVM.AST.Type as LLVM (Type(..))
import qualified LLVM.AST.Operand as LLVM (Operand(..))
import qualified LLVM.AST.Constant as LLVM (Constant(..))

data Types :: [LlvmType] -> Haskell.Type where
    TNil :: Types '[]
    TCons :: Type a -> Types as -> Types (a ': as)

class ToList as where
    toList :: Types as -> [LLVM.Type]

instance ToList '[] where
    toList _ = [] 

instance ToList as => ToList (a ': as) where
    toList (TCons head tail) = llvmType head : toList tail

type1 :: Type a -> Types '[a]
type1 a = a `TCons` TNil

type2 :: Type a -> Type b -> Types '[a, b]
type2 a b = a `TCons` type1 b

type3 :: Type a -> Type b -> Type c -> Types '[a, b, c]
type3 a b c = a `TCons` type2 b c

data LlvmType
    = Void
    | Int Nat
    | Pointer LlvmType
    | Function [LlvmType] LlvmType
    | Struct [LlvmType]
    | Vector Nat LlvmType
    | Array Nat LlvmType
    | Metadata
    | Label
    | Token

type args :-> ret = Function args ret

newtype Type (ty :: LlvmType) = Type { llvmType :: LLVM.Type }
    deriving (Show, Eq)

lowerOperand :: Operand ty -> LLVM.Operand
lowerOperand = coerce

voidType :: Type Void
voidType = Type LLVM.VoidType

knownNatToWord32 :: forall n proxy. KnownNat n => proxy n -> Word32
knownNatToWord32 proxy = fromInteger $ natVal proxy

integerType :: forall n. KnownNat n => Type (Int n)
integerType = Type . LLVM.IntegerType $ knownNatToWord32 (Proxy :: Proxy n)

i1 :: Type (Int 1)
i1 = integerType @1

i8 :: Type (Int 8)
i8 = integerType @8

i32 :: Type (Int 32)
i32 = integerType @32

pointerTo :: Type ty -> Type (Pointer ty)
pointerTo (Type ty) = Type (LLVM.PointerType ty (LLVM.AddrSpace 0))

functionType :: ToList args => Types args -> Type ret -> Type (args :-> ret)
functionType argTypes retType = Type (LLVM.FunctionType (llvmType retType) (toList argTypes) False)

structType :: ToList elements => Types elements -> Type (Struct elements)
structType elementTypes = Type (LLVM.StructureType False (toList elementTypes))

vectorType :: forall n ty. KnownNat n => Type ty -> Type (Vector n ty)
vectorType elementType = Type (LLVM.VectorType (knownNatToWord32 (Proxy :: Proxy n)) (llvmType elementType))

arrayType :: forall n ty. KnownNat n => Type ty -> Type (Vector n ty)
arrayType elementType = Type (LLVM.ArrayType (fromInteger $ natVal (Proxy :: Proxy n)) (llvmType elementType))

metadataType :: Type Metadata
metadataType = Type LLVM.MetadataType

labelType :: Type Label
labelType = Type LLVM.LabelType

tokenType :: Type Token
tokenType = Type LLVM.TokenType

newtype Operand (ty :: LlvmType) = Operand LLVM.Operand
newtype Constant (ty :: LlvmType) = Constant LLVM.Constant

