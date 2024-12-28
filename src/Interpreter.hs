-- src/Interpreter.hs
module Interpreter where

import Types
import Commands
import Parser
import Debug.Trace
import Control.Monad.IO.Class (liftIO)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Функция для выполнения программы (например, списка команд)
runColonProgram :: [String] -> ColonState (Either EvalError ())
runColonProgram [] = return (Right ())  -- Если команды кончились, завершаем программу
runColonProgram (cmd:cmds) = do
    -- Парсим команду
    let parseResult = parse parseCommand "" cmd

    -- Нужно для отладки
    liftIO $ putStrLn $ "Raw input: " ++ cmd
    liftIO $ putStrLn $ "Parse result: " ++ show parseResult

    case parseResult of
        Left parseError -> do
            liftIO $ putStrLn $ "Parse error: " ++ show parseError
            return $ Left (InvalidCommand (show parseError))  -- Возвращаем ошибку парсинга
        Right command -> do
            -- Если парсинг успешен, выполняем команду
            result <- execute command
            case result of
                Left err  -> return (Left err)  -- Если ошибка выполнения, возвращаем её
                Right _   -> runColonProgram cmds  -- Иначе продолжаем выполнение следующих команд