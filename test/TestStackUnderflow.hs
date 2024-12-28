module TestStackUnderflow (testStackUnderflow) where

import Test.Tasty
import Test.Tasty.HUnit
import Commands
import Control.Monad.State

testStackUnderflow :: TestTree
testStackUnderflow = testCase "Should return StackUnderflow error when stack is empty" $ do
    let initialState = []
    let command = Add
    let result = execState (execute command) initialState
    result @?= Left StackUnderflow