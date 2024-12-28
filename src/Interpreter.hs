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
runColonProgram :: [Command] -> ColonState (Either EvalError ())
runColonProgram [] = return (Right ())  -- Если команды кончились, завершаем программу
runColonProgram (cmd:cmds) = do
    liftIO $ putStrLn $ "Executing command: " ++ show cmd
    result <- execute cmd  -- Выполняем команду

    case result of
        Left err  -> return (Left err)  -- Если ошибка выполнения, возвращаем её
        Right _   -> runColonProgram cmds  -- Иначе продолжаем выполнение следующих команд