module LLVM.Strong.IRBuilder.Constant where

import Prelude hiding (Int)

import LLVM.Strong.AST.Constant (int, int')
import LLVM.Strong.AST.Operand (Operand, constant)
import LLVM.Strong.AST.Type (LlvmType(..))

bit :: Integer -> Operand (Int 1)
bit = constant . int' @1

int8 :: Integer -> Operand (Int 8)
int8 = constant . int' @8

int16 :: Integer -> Operand (Int 16)
int16 = constant . int' @16

int32 :: Integer -> Operand (Int 32)
int32 = constant . int' @32

int64 :: Integer -> Operand (Int 64)
int64 = constant . int' @64
