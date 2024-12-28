-- src/Types.hs
module Types where

import Control.Exception (Exception)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Except (ExceptT)

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
  deriving (Show, Eq)

--  | If [Command] [Command]   -- Условный оператор If-Else
--  | IfDoLoop [Command]       -- Циклическая конструкция

-- Тип для ошибок выполнения
data EvalError = StackUnderflow | DivisionByZero | InvalidCommand String
  deriving (Show, Eq)

-- Делаем EvalError экземпляром класса Exception
instance Exception EvalError

-- Тип стека — это просто список целых чисел
type Stack = [Int]

-- Тип вычислительного состояния — это StateT для работы с состоянием (стеком),
-- и IO для выполнения операций ввода/вывода.
type ColonState = StateT Stack IO -- еще Dictionary туда добавляетс
