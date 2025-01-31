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
import Control.Monad.State

-- Парсер для списка команд
parseCommands :: Parser [Command]
parseCommands = many (spaces >> parseCommand)

-- Парсер строковых команд
parseKeyword :: String -> Command -> Parser Command
parseKeyword keyword cmd = string keyword >> return cmd

-- Функция для парсинга символов, разрешённых в именах слов
validWordChar :: Parser Char
validWordChar = alphaNum <|> oneOf "?-_"

parseNumber :: Parser Command
parseNumber = do
    num <- many1 digit  -- Считываем одно или более цифр

    -- trace ("Parsed digits: " ++ num) (return ())

    return (PushToStack (read num))  -- Преобразуем строку в число и возвращаем команду

-- Парсер IF ... ELSE ... THEN
parseIfElse :: Parser Command
parseIfElse = do
    _ <- string "IF"
    spaces
    trueBranch <- manyTill (spaces >> parseCommand) (try (spaces >> (string "ELSE" <|> string "THEN")))

    trace ("Parsed trueBranch: " ++ show trueBranch) (return ())

    spaces
    hasElse <- optionMaybe (try (string "ELSE"))
    falseBranch <- case hasElse of
        Just _  -> manyTill (spaces >> parseCommand) (try (spaces >> string "THEN"))
        Nothing -> return []
    return (IfElse trueBranch falseBranch)

-- Парсер DO ... I ... LOOP
parseLoop :: Parser Command
parseLoop = do
    _ <- string "DO"
    spaces
    loopBody <- manyTill (spaces >> parseCommand) (try (spaces >> string "LOOP"))
    return (Loop loopBody)

parsePrintLiteralString :: Parser Command
parsePrintLiteralString = do
    _ <- string "."  -- Начало строки
    nextChar <- optionMaybe anyChar  -- Смотрим следующий символ, но он может не быть

    case nextChar of
        Just '"' -> do  -- Если следующий символ кавычка
            str <- manyTill anyChar (char '"')  -- Считываем символы до закрывающей кавычки

            return (PrintLiteralString str)  -- Возвращаем команду для печати строки
        _ -> do
            -- Если нет кавычки после точки, просто возвращаем команду PrintTop
            return PrintTop  -- Возвращаем команду для печати точки

-- Парсер для комментариев
parseComment :: Parser Command
parseComment = do
    _ <- char '('                   -- Открывающая скобка
    content <- many (noneOf "()")   -- Текст комментария

    trace ("Parsed digits: " ++ content) (return ())
    subComments <- many parseComment -- Рекурсивно обрабатываем вложенные комментарии
    _ <- char ')'                   -- Закрывающая скобка
    return $ Comment (content ++ concatMap show subComments)

parseCase :: Parser Command
parseCase = do
    _ <- string "CASE"
    spaces
    value <- parseNumberValue  -- Считываем значение для CASE
    branches <- many parseBranch  -- Считываем ветки
    _ <- string "ENDCASE"
    return (Case branches value)

-- Парсер для ветки CASE
parseBranch :: Parser (Either Int Double, [Command])
parseBranch = do
    spaces
    value <- parseNumberValue  -- Считываем значение
    _ <- string "OF"
    actions <- many (spaces >> parseCommand)  -- Считываем действия
    _ <- string "ENDOF"
    return (value, actions)

parseNumberValue :: Parser (Either Int Double)
parseNumberValue = try (Right <$> parseFloat) <|> (Left <$> parseInteger)

parseNewWord :: Parser Command
parseNewWord = do
    _ <- string ":"  -- Начало определения нового слова

    spaces
    word <- many1 validWordChar  -- Название нового слова
    -- trace ("Parsed word: " ++ word) (return ()) - проверка парсинга слова
    spaces

    commands <- manyTill (spaces >> choice [try parseIfElse, try parseLoop, try parseBeginUntil, parseCommand]) (try (spaces >> char ';'))
    -- trace ("Parsed commands: " ++ show commands) (return ()) - провека парсинга команд
    return (DefineWord word commands)

parseBeginUntil :: Dictionary -> Parser Command
parseBeginUntil = do
    _ <- string "BEGIN"
    spaces
    commands <- manyTill parseCommand (try (spaces >> string "UNTIL"))
    return (ExecuteCommands commands)

-- Парсер для всех команд
parseCommand :: Parser Command
parseCommand = 
    try parseNewWord
    <|> try parseComment
    <|> try parseBranch
    <|> try parseCase
    <|> try parsePrintLiteralString
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
    <|> try (parseKeyword "CR" Cr)
    <|> try (parseKeyword "EMIT" Emit)
    <|> try (parseKeyword "CREATE" Create)
    <|> try (parseKeyword "CELLS" Cells)
    <|> try (parseKeyword "ALLOT" Allot)
    <|> try (parseKeyword "!" Store)
    <|> try (parseKeyword "@" Fetch)
    <|> try (parseKeyword "LEAVE" Leave)
    <|> try (parseKeyword "+LOOP" PlusLoop)
    <|> try parseNumberValue
    <|> try parseNumber
