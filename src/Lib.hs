{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where
        
import Data.Text.Lazy.IO as T

import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

someFunc :: IO ()
someFunc = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ do
    function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
        entry <- block `named` "entry"; do
            c <- add a b
            ret c
