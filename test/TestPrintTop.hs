module TestPrintTop (testPrintTop) where

import System.IO.Silently (capture)
import Test.Tasty
import Test.Tasty.HUnit
import Commands
import Control.Monad.State

testPrintTop :: TestTree
testPrintTop = testCase "PrintTop should print the top element of the stack" $ do
    let initialState = [10, 20, 30]
    let command = PrintTop
    (output, _) <- capture $ execState (execute command) initialState
    output @?= "30\n"