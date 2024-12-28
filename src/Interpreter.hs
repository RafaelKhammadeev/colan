-- src/Interpreter.hs
module Interpreter where

import Types
import Commands
import Parser
import Debug.Trace
import Control.Monad.IO.Class (liftIO)

-- Функция для выполнения программы (например, списка команд)
runColonProgram :: [String] -> ColonState (Either EvalError ())
runColonProgram [] = return (Right ())  -- Если команды кончились, завершаем программу
runColonProgram (cmd:cmds) = do

    -- нужно для отладки
    let command = parseCommand cmd  -- Парсим команду
    liftIO $ putStrLn $ "Parsed command: " ++ show command  -- Выводим результат parseCommand
    result <- execute command  -- Выполняем команду

    -- рабочая лошадь
    -- let command = parseCommand cmd  -- Парсим команду
    -- result <- execute command  -- Выполняем команду

    case result of
        Left err  -> return (Left err)  -- Если ошибка, возвращаем её
        Right _   -> runColonProgram cmds  -- Иначе продолжаем выполнение следующих команд