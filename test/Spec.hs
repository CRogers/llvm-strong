{-# OPTIONS_GHC -fdefer-type-errors #-}

import Test.Hspec (hspec, describe, it)
import Test.ShouldNotTypecheck (shouldNotTypecheck)

main :: IO ()
main = hspec $ do
    describe "Type Tests" $ do
        it "should not typecheck" $ do
            shouldNotTypecheck ()
