module Main where

import Types
import Parser
import Interpreter
import Control.Monad.State
import Text.Parsec
import qualified Data.Map as Map

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
            -- liftIO $ putStrLn $ "Parse result: " ++ show parseResult   

            case parseResult of
                Left err -> do
                    liftIO $ print err  -- Ошибка парсинга

                Right commands -> do
                    (_, dict) <- get  -- Получаем текущий стек и словарь
                    case Map.lookup input dict of
                        Just dict_command -> do
                            liftIO $ putStrLn $ "Dict command: " ++ show dict_command
                            -- Если слово есть в словаре, выполняем соответствующие команды
                            result <- runColonProgram dict_command
                            case result of
                                Left evalError -> liftIO $ print evalError
                                Right _ -> liftIO $ putStrLn "ok"
                        Nothing -> do
                            -- Если команды нет в словаре, выполняем распарсенные команды
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
