{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}


module LLVM.Strong.AST.Instruction where

import Data.Singletons.TH (singletons)
import Data.Nat (Nat(..), Lit, sLit)

$(singletons [d|

    data Type 
        = Void
        | Integer Nat

    |])

voidType :: SType 'Void
voidType = SVoid

i1 :: SType ('Integer (Lit 1))
i1 = SInteger (sLit @1)