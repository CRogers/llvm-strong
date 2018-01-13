{-# LANGUAGE PartialTypeSignatures #-}

module LLVM.Strong.IRBuilder.Instruction where

import Prelude hiding (Int)

import qualified LLVM.AST.Operand as LLVM (Operand)
import qualified LLVM.AST.ParameterAttribute as LLVM (ParameterAttribute(..))
import LLVM.IRBuilder.Monad (MonadIRBuilder)
import qualified LLVM.IRBuilder.Instruction as LLVM

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))
import LLVM.Strong.AST.Internal.TypeIndexedList (TypeIndexedList, list1)
import LLVM.Strong.AST.Constant (undef)
import LLVM.Strong.AST.Operand (Operand(..), constant)
import LLVM.Strong.AST.Type (LlvmType(..), Type, (:->), i8, i32, pointerTo, functionType)
import LLVM.Strong.IRBuilder.Constant (int8, int32)

binop ::
    MonadIRBuilder m => 
    (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand)
    -> Operand a
    -> Operand b
    -> m (Operand c)
binop f a b = Operand <$> f (lower a) (lower b)

add, mul, sub, udiv, sdiv, urem, frem :: MonadIRBuilder m => Operand (Int n) -> Operand (Int n) -> m (Operand (Int n))
add = binop LLVM.add
mul = binop LLVM.mul
sub = binop LLVM.sub
udiv = binop LLVM.udiv
sdiv = binop LLVM.sdiv
urem = binop LLVM.urem
frem = binop LLVM.frem

alloca :: MonadIRBuilder m => Type ty -> Maybe (Operand (Int i)) -> Integer -> m (Operand (Pointer ty))
alloca ty numElements alignment = Operand <$> LLVM.alloca (lower ty) (lower <$> numElements) (fromInteger alignment)

load :: MonadIRBuilder m => Operand (Pointer ty) -> m (Operand ty)
load pointer = Operand <$> LLVM.load (lower pointer) 0

store :: MonadIRBuilder m => Operand ty -> Operand (Pointer ty) -> m ()
store value pointer = LLVM.store (lower value) 0 (lower pointer)

inttoptr :: MonadIRBuilder m => Operand (Int i) -> Type (Pointer p) -> m (Operand (Pointer p))
inttoptr int pointerType = Operand <$> LLVM.inttoptr (lower int) (lower pointerType)

type LoweredArg = (LLVM.Operand, [LLVM.ParameterAttribute])
newtype Arg (ty :: LlvmType) = Arg LoweredArg

instance Lowerable Arg where
    type Lower Arg = LoweredArg
    lower (Arg lowered) = lowered

type Args = TypeIndexedList Arg

argOf :: Operand ty -> [LLVM.ParameterAttribute] -> Arg ty
argOf operand attributes = Arg $ (lower operand, attributes)

call :: MonadIRBuilder m => Operand (args :-> ret) -> Args args -> m (Operand ret)
call f args = Operand <$> LLVM.call (lower f) (lower args)

foo :: MonadIRBuilder m => m _
foo = do
    let x = int8 4
    let y = int8 5
    v <- add x y
    pointer <- alloca i8 (Just $ int32 1) 0
    -- pointer <- inttoptr (constant (int i32 12)) (pointerTo i8)
    store v pointer
    load pointer

    call (constant $ undef $ functionType (list1 i8) i32) (list1 (argOf x []))  

    -- let f = constant $ undef $ 