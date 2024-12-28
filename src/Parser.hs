module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string, spaces, letter, anyChar)
import Text.Parsec.Combinator (many1, skipMany1)
import Data.Char (isDigit)
import Types (Command(..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Types (Command(..))  -- Импортируем только Command из модуля Types
import Debug.Trace (trace)

parseNumber :: Parser Command
parseNumber = do
    num <- many1 digit  -- Считываем одно или более цифр

    -- trace ("Parsed digits: " ++ num) (return ())

    return (PushToStack (read num))  -- Преобразуем строку в число и возвращаем команду

-- Парсер строковых команд
parseKeyword :: String -> Command -> Parser Command
parseKeyword keyword cmd = string keyword >> return cmd

-- Парсер для всех команд
parseCommand :: Parser Command
parseCommand = 
    -- Используем try, чтобы сначала проверять более длинные команды
    try (parseKeyword "DROP" Drop)  -- DROP должна быть проверена первой
    <|> try (parseKeyword "DUP" Dup)  -- DUP должна быть проверена второй
    <|> try (parseKeyword "+" Add)
    <|> try (parseKeyword "-" Subtract)
    <|> try (parseKeyword "*" Multiply)
    <|> try (parseKeyword "/" Divide)
    <|> try (parseKeyword "MOD" Mod)
    <|> try (parseKeyword "SWAP" Swap)
    <|> try (parseKeyword "OVER" Over)
    <|> try (parseKeyword "ROT" Rot)
    <|> try (parseKeyword "=" Eq)
    <|> try (parseKeyword "<" Lt)
    <|> try (parseKeyword ">" Gt)
    <|> try (parseKeyword "AND" And)
    <|> try (parseKeyword "OR" Or)
    <|> try (parseKeyword "INVERT" Invert)
    <|> try (parseKeyword "." PrintTop)
    <|> try (parseKeyword "CR" Cr)
    <|> try (parseKeyword "EMIT" Emit)
    <|> parseNumber  -- Если ничего не подошло, проверяем число

-- Парсер для списка команд
parseCommands :: Parser [Command]
parseCommands = many parseCommand