module LLVM.Strong.AST.Constant where

import Prelude hiding (Int)

import GHC.TypeLits (Nat, KnownNat, natVal, type (+))
import qualified Data.Kind as Haskell (Type)
import Data.Proxy (Proxy(..))

import qualified LLVM.AST.Constant as LLVM (Constant(..))
import qualified LLVM.AST.Type as LLVM (typeBits)

import LLVM.Strong.AST.Type (LlvmType(..), Type, lowerType)

newtype Constant (ty :: LlvmType) = Constant { lowerConstant :: LLVM.Constant }


data Constants :: Nat -> [LlvmType] -> Haskell.Type where
    CNil :: Constants 0 '[]
    CCons :: Constant a -> Constants n as -> Constants (n + 1) (a ': as)

class ToList as where
    toList :: Constants n as -> [LLVM.Constant]

instance ToList '[] where
    toList _ = [] 

instance ToList as => ToList (a ': as) where
    toList (CCons head tail) = lowerConstant head : toList tail


int :: forall n. KnownNat n => Integer -> Constant (Int n)
int number = Constant (LLVM.Int (fromInteger $ natVal (Proxy :: Proxy n)) number)

intFromTy :: Type (Int n) -> Integer -> Constant (Int n)
intFromTy intTy number = Constant (LLVM.Int (LLVM.typeBits $ lowerType intTy) number)

null :: Type ty -> Constant ty
null ty = Constant (LLVM.Null $ lowerType ty)

struct :: ToList elements => Constants n elements -> Constant (Struct elements)
struct elementConstants = Constant (LLVM.Struct Nothing False $ toList elementConstants)

array :: Type ty -> [Constant ty] -> Constant (Array n ty)
array ty constants = Constant (LLVM.Array (lowerType ty) (map lowerConstant constants))

vector :: [Constant ty] -> Constant (Vector n ty)
vector constants = Constant (LLVM.Vector (map lowerConstant constants))

undef :: Type ty -> Constant ty
undef ty = Constant $ LLVM.Undef (lowerType ty)