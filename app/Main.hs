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
import Text.Parsec
import Text.Parsec.String (Parser)

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

    -- Выводим введенную команду
    liftIO $ putStrLn $ "Raw input: " ++ input

    case input of
        "quit" -> liftIO $ putStrLn "Goodbye!"
        _      -> do
            -- Парсинг команд
            let parseResult = parse parseCommands "" input

            -- Выводим результат парсинга
            liftIO $ putStrLn $ "Parse result: " ++ show parseResult   

            case parseResult of
                Left err -> liftIO $ print err  -- Ошибка парсинга

                Right commands -> do
                    result <- runColonProgram commands  -- Теперь передаем список строк
                    case result of
                        Left evalError -> liftIO $ print evalError
                        Right _ -> liftIO $ putStrLn "ok"
            printStack  -- Печатаем текущий стек
            repl

main :: IO ()
main = do
    putStrLn "Welcome to Colon REPL!"
    putStrLn "Type commands or 'quit' to quit."
    evalStateT repl []
