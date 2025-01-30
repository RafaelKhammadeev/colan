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
    try parseNewWord
    <|> try parseComment
    <|> try (parseKeyword "DROP" Drop)
    <|> try (parseKeyword "DUP" Dup)
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
    <|> parseNumber

parseNewWord :: Parser Command
parseNewWord = do
    _ <- string ":"  -- Начало определения нового слова
    word <- many1 letter  -- Название нового слова

    trace ("Parsed digits: " ++ word) (return ())

    commands <- many parseCommand  -- Список команд для нового слова
    _ <- string ";"  -- Завершающая точка с запятой
    return (DefineWord word commands)

-- Парсер для комментариев
parseComment :: Parser Command
parseComment = do
    _ <- char '('                   -- Открывающая скобка
    content <- many (noneOf "()")   -- Текст комментария

    trace ("Parsed digits: " ++ content) (return ())
    subComments <- many parseComment -- Рекурсивно обрабатываем вложенные комментарии
    _ <- char ')'                   -- Закрывающая скобка
    return $ Comment (content ++ concatMap show subComments)


-- Парсер для списка команд
parseCommands :: Parser [Command]
parseCommands = many (spaces >> parseCommand)