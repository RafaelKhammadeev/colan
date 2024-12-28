module Main where

import Test.Tasty
import TestPushToStack (testPushToStack)
import TestStackUnderflow (testStackUnderflow)
import TestPrintTop (testPrintTop)
import TestEmitValid (testEmitValid)
import TestRepl (testRepl)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Colon Commands Tests"
  [ testPushToStack
  , testStackUnderflow
  , testPrintTop
  , testEmitValid
  , testRepl
  ]