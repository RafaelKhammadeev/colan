-- src/Parser.hs
module Parser where

import Types
import Data.Char (isDigit)

-- Функция для парсинга команд
parseCommand :: String -> Command
parseCommand "+" = Add
parseCommand "-" = Subtract
parseCommand "*" = Multiply
parseCommand "/" = Divide
parseCommand "MOD" = Mod
parseCommand "SWAP" = Swap
parseCommand "DUP" = Dup
parseCommand "DROP" = Drop
parseCommand "OVER" = Over
parseCommand "ROT" = Rot
parseCommand "="    = Eq
parseCommand "<"    = Lt
parseCommand ">"    = Gt
parseCommand "AND"  = And
parseCommand "OR"   = Or
parseCommand "INVERT" = Invert
parseCommand "."           = PrintTop
parseCommand "CR"          = Cr
parseCommand "EMIT"        = Emit
parseCommand ('.':xs)      = PrintString xs  -- Обработка строк
parseCommand "KEY"         = Key

parseCommand s
  | head s == '-' && all isDigit (tail s) = PushToStack (read s)  -- Обрабатываем отрицательные числа
  | all isDigit s = PushToStack (read s)  -- Если это число, то команда Push с этим числом
  | otherwise     = Invalid s      -- Если команда не распознана, то Invalid с ошибкой