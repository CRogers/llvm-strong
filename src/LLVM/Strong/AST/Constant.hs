module LLVM.Strong.AST.Constant where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal, type (+), type (-))
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))

import qualified LLVM.AST.Constant as LLVM (Constant(..))
import qualified LLVM.AST.Type as LLVM (typeBits)

import LLVM.Strong.AST.Type (LlvmType(..), Type, lowerType)

newtype Constant (ty :: LlvmType) = Constant { lowerConstant :: LLVM.Constant }

type family Lower (f :: LlvmType -> Haskell.Type) :: Haskell.Type

data TypeIndexedList :: (LlvmType -> Haskell.Type) -> [LlvmType] -> Haskell.Type where
    TILNil :: TypeIndexedList f '[]
    TILCons :: f a -> TypeIndexedList f as -> TypeIndexedList f (a ': as)

type Constants = TypeIndexedList Constant
type instance Lower Constant = LLVM.Constant

lowerTypeIndexedList :: (forall a. f a -> Lower f) -> TypeIndexedList f vals -> [Lower f]
lowerTypeIndexedList f constants = case constants of
    TILNil -> []
    TILCons c cs -> f c : lowerTypeIndexedList f cs

lowerConstants :: Constants vals -> [LLVM.Constant]
lowerConstants = lowerTypeIndexedList lowerConstant

data SizedList :: Nat -> Haskell.Type -> Haskell.Type where
    SLNil :: SizedList 0 a
    SLCons :: a -> SizedList n a -> SizedList (n + 1) a

lowerSizedList :: SizedList n a -> [a]
lowerSizedList list = case list of
    SLNil -> []
    SLCons a as -> a : lowerSizedList as


int' :: forall n. KnownNat n => Integer -> Constant (Int n)
int' number = Constant (LLVM.Int (fromInteger $ natVal (Proxy :: Proxy n)) number)

int :: Type (Int n) -> Integer -> Constant (Int n)
int intTy number = Constant (LLVM.Int (LLVM.typeBits $ lowerType intTy) number)

null :: Type ty -> Constant ty
null ty = Constant (LLVM.Null $ lowerType ty)

struct :: Constants elements -> Constant (Struct elements)
struct constants = Constant (LLVM.Struct Nothing False $ lowerConstants constants)

array :: Type ty -> SizedList n (Constant ty) -> Constant (Array n ty)
array ty constants = Constant (LLVM.Array (lowerType ty) (map lowerConstant $ lowerSizedList constants))

vector :: SizedList n (Constant ty) -> Constant (Vector n ty)
vector constants = Constant (LLVM.Vector (map lowerConstant $ lowerSizedList constants))

undef :: Type ty -> Constant ty
undef ty = Constant $ LLVM.Undef (lowerType ty)