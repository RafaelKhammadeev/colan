module Commands where

import Types
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (throwE)

splitLastTwo :: [a] -> Maybe ([a], a, a)
splitLastTwo xs
  | length xs >= 2 = Just (init initList, last initList, last xs)
  | otherwise      = Nothing
  where initList = init xs

splitLastThree :: [a] -> Maybe ([a], a, a, a)
splitLastThree xs
  | length xs >= 3 = Just (init (init initList), last (init initList), last initList, last xs)
  | otherwise      = Nothing
  where initList = init xs

-- Функции для выполнения команд
execute :: Command -> ColonState (Either EvalError ())
execute (PushToStack n) = modify (\stack -> stack ++ [n]) >> return (Right ())

execute Add = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [x + y])  -- Складываем последние два элемента и кладем результат в конец стека
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Subtract = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [x - y])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Multiply = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [x * y])  -- Умножаем последние два элемента и кладем результат в конец стека
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Divide = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            if y == 0
                then return (Left DivisionByZero)  -- Возвращаем ошибку, если делим на ноль
                else do
                    put (xs ++ [x `div` y])  -- Делим последние два элемента и кладем результат в конец стека
                    return (Right ())  -- Возвращаем успешный результат
        Nothing -> return (Left StackUnderflow)  -- Возвращаем ошибку, если недостаточно элементов

execute Mod = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> 
            if y == 0 
                then return (Left DivisionByZero)  -- Ошибка деления на ноль
                else do
                    put (xs ++ [x `rem` y])  -- Остаток от деления
                    return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Swap = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [y, x])  -- Умножаем последние два элемента и кладем результат в конец стека
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Dup = do
    stack <- get
    case stack of
        [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
        stack -> do
            let last_elem = last stack  -- Получаем последний элемент
            put (stack ++ [last_elem])  -- Добавляем его в конец стека
            return (Right ())

execute Drop = do
    stack <- get
    case stack of
        [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
        stack -> do
            put (init stack)  -- Добавляем его в конец стека
            return (Right ())

execute Over = do
    stack <- get
    if length stack < 2
        then return (Left StackUnderflow)  -- Ошибка, если элементов меньше 3
        else do
            let secondToLast = last (init stack) -- Предпоследний элемент
            put (stack ++ [secondToLast]) -- Переставляем элементы
            return (Right ())

execute Rot = do
    stack <- get
    case splitLastThree stack of
        Just (rest, x, y, z) -> do
            put (rest ++ [y, z, x]) -- Поменяли местами последние три элемента
            return (Right ())
        Nothing -> return (Left StackUnderflow)

-- Реализация операций сравнения и булевых операций

execute Eq = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [if x == y then -1 else 0])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Lt = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [if x < y then -1 else 0])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Gt = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [if x > y then -1 else 0])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute And = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [if x /= 0 && y /= 0 then -1 else 0])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Or = do
    stack <- get
    case splitLastTwo stack of
        Just (xs, x, y) -> do
            put (xs ++ [if x /= 0 || y /= 0 then -1 else 0])
            return (Right ())
        Nothing -> return (Left StackUnderflow)

execute Invert = do
    stack <- get
    case stack of
        [] -> return (Left StackUnderflow)
        xs -> do
            let (initStack, lastElem) = (init xs, last xs)
            put (initStack ++ [if lastElem == 0 then -1 else 0])
            return (Right ())

-- Функция для выполнения команд ввода и вывода

execute PrintTop = do
    stack <- get
    case stack of
        [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
        stack -> do
            let upper_item = last stack
            liftIO (print upper_item) >> return (Right ())  -- Печать верхнего элемента стека
            put $ init stack
            return (Right ())

execute Cr = do
    liftIO (putStrLn "")  -- Печать новой строки
    return (Right ())

execute Emit = do
    stack <- get
    case stack of
        [] -> return (Left StackUnderflow)  -- Если стек пуст, ошибка
        stack -> do
            let last_elem = last stack  -- Последний положенный элемент
            if last_elem >= 0 && last_elem <= 255
                then do
                    liftIO (putChar (toEnum last_elem))  -- Печатаем символ, соответствующий ASCII коду
                    return (Right ())  -- Возвращаем успешный результат
                else return (Left $ InvalidCommand "Value out of ASCII range")

execute (PrintString str) = do
    liftIO (putStrLn str)  -- Печать строки
    return (Right ())

execute Key = do
    c <- liftIO getChar  -- Ожидание ввода символа
    put [fromEnum c]      -- Кладем код символа в стек
    return (Right ())
