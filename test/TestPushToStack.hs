module TestPushToStack (testPushToStack) where

import Test.Tasty
import Test.Tasty.HUnit
import Commands
import Control.Monad.State

testPushToStack :: TestTree
testPushToStack = testCase "PushToStack command should push value to stack" $ do
    let initialState = []
    let command = PushToStack 10
    let result = execState (execute command) initialState
    result @?= [10]