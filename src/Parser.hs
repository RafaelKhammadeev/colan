module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string, spaces, letter, anyChar)
import Text.Parsec.Combinator (many1, skipMany1)
import Data.Char (isDigit)
import Types (Command(..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)

parseNumber :: Parser Command
parseNumber = do
    num <- many1 digit  -- Считываем одно или более цифр
    return (PushToStack (read num))  -- Преобразуем строку в число и возвращаем команду

-- Парсер для пробелов
parseSpaces :: Parser ()
parseSpaces = void $ skipMany1 spaces  -- Пропускаем все пробелы

-- Парсер строковых команд
parseKeyword :: String -> Command -> Parser Command
parseKeyword keyword cmd = string keyword >> return cmd

-- Парсер для всех команд
parseCommand :: Parser Command
parseCommand = parseSpaces *> (
    parseNumber  -- Парсинг числа
    <|> parseKeyword "+" Add
    <|> parseKeyword "-" Subtract
    <|> parseKeyword "*" Multiply
    <|> parseKeyword "/" Divide
    <|> parseKeyword "MOD" Mod
    <|> parseKeyword "SWAP" Swap
    <|> parseKeyword "DUP" Dup
    <|> parseKeyword "DROP" Drop
    <|> parseKeyword "OVER" Over
    <|> parseKeyword "ROT" Rot
    <|> parseKeyword "=" Eq
    <|> parseKeyword "<" Lt
    <|> parseKeyword ">" Gt
    <|> parseKeyword "AND" And
    <|> parseKeyword "OR" Or
    <|> parseKeyword "INVERT" Invert
    <|> parseKeyword "." PrintTop
    <|> parseKeyword "CR" Cr
    <|> parseKeyword "EMIT" Emit
    )

-- Парсер для списка команд
parseCommands :: Parser [Command]
parseCommands = many1 parseCommand