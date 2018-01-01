{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec (hspec, describe, it)

main :: IO ()
main = hspec $ do
    describe "StrongIRBuilder" $ do
        it "should make a function that can add two i32s together" $ do
            let pureModule = buildModule "exampleModule" $ do
                function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
                    block `named` "entry"; do
                        c <- add a b
                        ret c

            runM
