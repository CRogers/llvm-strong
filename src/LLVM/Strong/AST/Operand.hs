module LLVM.Strong.AST.Operand where

import qualified LLVM.AST.Operand as LLVM (Operand(..))
import qualified LLVM.AST.Name as LLVM (Name)

import LLVM.Strong.AST.Type (LlvmType, Type, lowerType)

newtype Operand (ty :: LlvmType) = Operand { lowerOperand :: LLVM.Operand }

localReference :: Type ty -> LLVM.Name -> Operand ty
localReference ty name = Operand (LLVM.LocalReference (lowerType ty) name)