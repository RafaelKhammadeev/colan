-- src/Types.hs
module Types where

import Control.Exception (Exception)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Map as Map

-- Делаем EvalError экземпляром класса Exception
instance Exception EvalError

-- Тип для команд
data Command
  = PushToStack Int          -- Поместить число в стек
  | Add                      -- Сложить два числа из стека
  | Subtract                 -- Вычесть два числа из стека
  | Multiply                 -- Умножить два числа из стека
  | Divide                   -- Разделить два числа из стека
  | Mod                      -- Остаток от деления
  | Swap                     -- Поменять местами два элемента в стеке
  | Dup                      -- Дублировать верхний элемент стека
  | Drop                     -- Удалить верхний элемент стека
  | Over                     -- Дублирование второго элемента с вершины стека
  | Rot                      -- Вращение трёх верхних элементов стека
  | Invalid String           -- Неправильная команда, которая возвращает строку с ошибкой
  | Eq                       -- равно
  | Lt                       -- меньше
  | Gt                       -- больше
  | And                      -- И
  | Or                       -- ИЛИ
  | Invert                   -- НЕ
  | PrintTop                 -- Вывод вершины стека
  | Cr                       -- Перевод строки
  | Emit                     -- Вывод символа
  | PrintString String       -- Вывод строки
  | Key                      -- Ввод символа
  | DefineWord String [Command]
  | Comment String           -- Комментарий (может быть вложенным)
  deriving (Show, Eq)

--  | If [Command] [Command]   -- Условный оператор If-Else
--  | IfDoLoop [Command]       -- Циклическая конструкция

-- Тип для ошибок выполнения
data EvalError 
  = StackUnderflow
  | DivisionByZero
  | InvalidCommand String
  | InvalidComment
  deriving (Show, Eq)

type Stack = [Int]
type Dictionary = Map.Map String [Command]

type ColonState a = StateT (Stack, Dictionary) IO a
