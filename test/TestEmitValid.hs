module TestEmitValid (testEmitValid) where

import Test.Tasty
import Test.Tasty.HUnit
import Commands
import Control.Monad.State

testEmitValid :: TestTree
testEmitValid = testCase "Emit should print the character corresponding to ASCII code" $ do
    let initialState = [65]  -- Код ASCII для 'A'
    (output, _) <- capture $ execState (execute Emit) initialState
    output @?= "A"