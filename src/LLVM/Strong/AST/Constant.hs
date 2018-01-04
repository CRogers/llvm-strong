module LLVM.Strong.AST.Constant where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal, type (+))
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))

import qualified LLVM.AST.Constant as LLVM (Constant(..))
import qualified LLVM.AST.Type as LLVM (typeBits)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))
import LLVM.Strong.AST.Internal.TypeIndexedList (TypeIndexedList)
import LLVM.Strong.AST.Internal.SizedList (SizedList)
import LLVM.Strong.AST.Type (LlvmType(..), Type)


newtype Constant (ty :: LlvmType) = Constant { lowerConstant :: LLVM.Constant }

type Constants = TypeIndexedList Constant

instance Lowerable Constant where
    type Lower Constant = LLVM.Constant
    lower = lowerConstant


int' :: forall n. KnownNat n => Integer -> Constant (Int n)
int' number = Constant (LLVM.Int (fromInteger $ natVal (Proxy :: Proxy n)) number)

int :: Type (Int n) -> Integer -> Constant (Int n)
int intTy number = Constant (LLVM.Int (LLVM.typeBits $ lower intTy) number)

null :: Type ty -> Constant ty
null ty = Constant (LLVM.Null $ lower ty)

struct :: Constants elements -> Constant (Struct elements)
struct constants = Constant (LLVM.Struct Nothing False $ lower constants)

array :: Type ty -> SizedList (Constant ty) n -> Constant (Array n ty)
array ty constants = Constant (LLVM.Array (lower ty) (map lowerConstant $ lower constants))

vector :: SizedList (Constant ty) n -> Constant (Vector n ty)
vector constants = Constant (LLVM.Vector (map lowerConstant $ lower constants))

undef :: Type ty -> Constant ty
undef ty = Constant $ LLVM.Undef (lower ty)