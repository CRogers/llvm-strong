module LLVM.Strong.AST.Operand where

import qualified LLVM.AST.Operand as LLVM (Operand(..))
import qualified LLVM.AST.Name as LLVM (Name)

import LLVM.Strong.AST.Internal.Lowerable (Lowerable(..))
import LLVM.Strong.AST.Type (LlvmType, Type)

newtype Operand (ty :: LlvmType) = Operand { lowerOperand :: LLVM.Operand }

instance Lowerable Operand where
    type Lower Operand = LLVM.Operand
    lower = lowerOperand

localReference :: Type ty -> LLVM.Name -> Operand ty
localReference ty name = Operand (LLVM.LocalReference (lower ty) name)