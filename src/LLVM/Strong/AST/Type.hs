module LLVM.Strong.AST.Type (
    LlvmType(..),
    Type,
    Types(..),
    voidType, integerType, i1, i8, i32, pointerTo, functionType, structType, vectorType, arrayType, metadataType, labelType, tokenType
    ) where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))

import Data.Word (Word32)

import qualified LLVM.AST.AddrSpace as LLVM (AddrSpace(..))
import qualified LLVM.AST.Type as LLVM (Type(..))

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))
import LLVM.Strong.AST.Internal.TypeIndexedList (TypeIndexedList)

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

newtype Type (ty :: LlvmType) = Type LLVM.Type
    deriving (Show, Eq)

instance Lowerable Type where
    type Lower Type = LLVM.Type
    lower (Type ty) = ty

type Types = TypeIndexedList Type

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

functionType :: Types args -> Type ret -> Type (args :-> ret)
functionType argTypes retType = Type (LLVM.FunctionType (lower retType) (lower argTypes) False)

structType :: Types elements -> Type (Struct elements)
structType elementTypes = Type (LLVM.StructureType False (lower elementTypes))

vectorType :: forall n ty. KnownNat n => Type ty -> Type (Vector n ty)
vectorType elementType = Type (LLVM.VectorType (knownNatToWord32 (Proxy :: Proxy n)) (lower elementType))

arrayType :: forall n ty. KnownNat n => Type ty -> Type (Vector n ty)
arrayType elementType = Type (LLVM.ArrayType (fromInteger $ natVal (Proxy :: Proxy n)) (lower elementType))

metadataType :: Type Metadata
metadataType = Type LLVM.MetadataType

labelType :: Type Label
labelType = Type LLVM.LabelType

tokenType :: Type Token
tokenType = Type LLVM.TokenType
