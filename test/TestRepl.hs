module TestRepl (testRepl) where

import System.IO.Silently (capture)

-- golden test написать
testRepl :: TestTree
testRepl = testCase "REPL should process commands and print output correctly" $ do
    -- Сначала вводим команду PushToStack, затем команду PrintTop
    let initialState = []
    (output, _) <- capture $ execStateT repl initialState
    output @?= "> ok\n| 10 <- Top\n> ok\n| 20 <- Top\n> "