module Main where

import Types
import Parser
import Interpreter
import Commands
import System.IO
import Debug.Trace
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

-- Добавляем функцию для отображения стека в удобочитаемом формате
printStack :: ColonState ()
printStack = do
    stack <- get
    liftIO $ putStrLn $ formatStack stack

-- Форматируем стек для вывода
formatStack :: [Int] -> String
formatStack stack = 
    let stackRepr = unwords (map show stack)
    in "| " ++ stackRepr ++ " <- Top"

-- Основной цикл REPL с выводом состояния стека
repl :: ColonState ()
repl = do
    liftIO $ putStr "> "
    input <- liftIO getLine
    case input of
        "quit" -> liftIO $ putStrLn "Goodbye!"
        _      -> do
            let commands = words input
            result <- runColonProgram commands  -- Выполняем команды
            case result of
                Left err  -> liftIO $ putStrLn $ show err  -- Отображаем ошибку
                Right _   -> liftIO $ putStrLn "ok"  -- Операция успешна
            printStack  -- Печатаем текущий стек
            repl

main :: IO ()
main = do
    putStrLn "Welcome to Colon REPL!"
    putStrLn "Type commands or 'quit' to quit."
    evalStateT repl []
