{-# LANGUAGE OverloadedStrings #-}

module Lib where
        
import Data.Text.Lazy.IO as T

import LLVM.Pretty (ppllvm)
import LLVM.AST (Module)
import LLVM.AST.Type (i32, void)

import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Module (buildModule, function)
import LLVM.IRBuilder.Monad (block, named)
import LLVM.IRBuilder.Instruction (add, ret)

import LLVM.Context (withContext)
import LLVM.Module (File(..), withModuleFromAST, writeObjectToFile)
import LLVM.Target (initializeAllTargets, withHostTargetMachine)

myModule :: Module
myModule = buildModule "exampleModule" $ do
    function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
        entry <- block `named` "entry"; do
            c <- add a b
            ret c

    function "main" [] i32 $ \[] -> do
        ret =<< int32 0

printModule :: IO ()
printModule = T.putStrLn $ ppllvm myModule

outputMachineCode :: IO ()
outputMachineCode = do
    initializeAllTargets
    withContext $ \context -> do
        withHostTargetMachine $ \targetMachine -> do
            withModuleFromAST context myModule $ \llvmModule ->
                writeObjectToFile targetMachine (File "foo") llvmModule