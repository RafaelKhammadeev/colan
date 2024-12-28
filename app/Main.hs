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
import qualified Data.Map as Map
import Text.Parsec.String (Parser)

-- Добавляем функцию для отображения стека в удобочитаемом формате
printStack :: ColonState ()
printStack = do
    (stack, _) <- get  -- Получаем только stack из состояния
    liftIO $ putStrLn $ formatStack stack

-- Форматируем стек для вывода
formatStack :: [Int] -> String
formatStack stack = 
    let stackRepr = unwords (map show stack)
    in "| " ++ stackRepr ++ " <- Top"

-- Функция для печати текущего словаря
printDictionary :: ColonState ()
printDictionary = do
    (_, dict) <- get
    liftIO $ putStrLn $ "Current dictionary: " ++ show dict

-- Основной цикл REPL с выводом состояния стека
repl :: ColonState ()
repl = do
    liftIO $ putStr "> "
    input <- liftIO getLine

    -- Выводим введенную команду
    liftIO $ putStrLn $ "Raw input: " ++ input

    case input of
        "quit" -> liftIO $ putStrLn "Goodbye!"
        _ -> do
            -- Парсинг команд
            let parseResult = parse parseCommands "" input

            -- Выводим результат парсинга
            liftIO $ putStrLn $ "Parse result: " ++ show parseResult   

            case parseResult of
                Left err -> do
                    liftIO $ print err  -- Ошибка парсинга

                    -- Если парсинг не удался, проверяем слово в словаре
                    (stack, dict) <- get  -- Получаем текущий стек и словарь
                    case Map.lookup input dict of
                        Just commands -> do
                            -- Если слово есть в словаре, выполняем соответствующие команды
                            result <- runColonProgram commands
                            case result of
                                Left evalError -> liftIO $ print evalError
                                Right _ -> liftIO $ putStrLn "ok"
                        Nothing -> liftIO $ putStrLn "Unknown command or word."  -- Если слова нет в словаре
                Right commands -> do
                    -- Если парсинг успешен, выполняем команду
                    result <- runColonProgram commands
                    case result of
                        Left evalError -> liftIO $ print evalError
                        Right _ -> liftIO $ putStrLn "ok"

            -- Печатаем текущий стек
            printStack
            printDictionary 
            repl

main :: IO ()
main = do
    putStrLn "Welcome to Colon REPL!"
    putStrLn "Type commands or 'quit' to quit."
    evalStateT repl ([], Map.empty)
