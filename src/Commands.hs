module Commands where

import Types
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Map as Map
import Data.List (isInfixOf)

-- Утилиты для работы со стеком

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

findMatchingBranch :: Either Int Double -> [(Either Int Double, [Command])] -> (Maybe [Command], [(Either Int Double, [Command])])
findMatchingBranch _ [] = (Nothing, [])
findMatchingBranch value ((branchValue, actions):rest) =
    if value == branchValue
    then (Just actions, rest)
    else let (matched, remaining) = findMatchingBranch value rest
         in (matched, (branchValue, actions) : remaining)

-- Функции для выполнения команд

execute :: Command -> ColonState (Either EvalError ())

-- Основные команды, работающие со стеком
execute cmd = case cmd of
    PushToStack n -> modify (\(stack, dict) -> (stack ++ [n], dict)) >> return (Right ())

    Add -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, Left x, Left y) -> put (xs ++ [Left (x + y)], dict) >> return (Right ())
            Just (xs, Right x, Right y) -> put (xs ++ [Right (x + y)], dict) >> return (Right ())
            Just (xs, Left x, Right y) -> put (xs ++ [Right (fromIntegral x + y)], dict) >> return (Right ())
            Just (xs, Right x, Left y) -> put (xs ++ [Right (x + fromIntegral y)], dict) >> return (Right ())
            Nothing -> return (Left StackUnderflow)

    Subtract -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, Left x, Left y) -> put (xs ++ [Left (x - y)], dict) >> return (Right ())
            Just (xs, Right x, Right y) -> put (xs ++ [Right (x - y)], dict) >> return (Right ())
            Just (xs, Left x, Right y) -> put (xs ++ [Right (fromIntegral x - y)], dict) >> return (Right ())
            Just (xs, Right x, Left y) -> put (xs ++ [Right (x - fromIntegral y)], dict) >> return (Right ())
            Nothing -> return (Left StackUnderflow)

    Multiply -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, Left x, Left y) -> put (xs ++ [Left (x * y)], dict) >> return (Right ())
            Just (xs, Right x, Right y) -> put (xs ++ [Right (x * y)], dict) >> return (Right ())
            Just (xs, Left x, Right y) -> put (xs ++ [Right (fromIntegral x * y)], dict) >> return (Right ())
            Just (xs, Right x, Left y) -> put (xs ++ [Right (x * fromIntegral y)], dict) >> return (Right ())
            Nothing -> return (Left StackUnderflow)

    Divide -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, Left x, Left y) -> 
                if y == 0 then return (Left DivisionByZero) 
                else put (xs ++ [Left (x `div` y)], dict) >> return (Right ())
            Just (xs, Right x, Right y) -> 
                if y == 0 then return (Left DivisionByZero) 
                else put (xs ++ [Right (x / y)], dict) >> return (Right ())
            Just (xs, Left x, Right y) -> 
                if y == 0 then return (Left DivisionByZero) 
                else put (xs ++ [Right (fromIntegral x / y)], dict) >> return (Right ())
            Just (xs, Right x, Left y) -> 
                if y == 0 then return (Left DivisionByZero) 
                else put (xs ++ [Right (x / fromIntegral y)], dict) >> return (Right ())
            Nothing -> return (Left StackUnderflow)

    Mod -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> 
                if y == 0 
                    then return (Left DivisionByZero)  -- Ошибка деления на ноль
                    else do
                        put (xs ++ [x `rem` y], dict)  -- Остаток от деления
                        return (Right ())
            Nothing -> return (Left StackUnderflow)

    Swap -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [y, x], dict)  -- Умножаем последние два элемента и кладем результат в конец стека
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    Dup -> do
        (stack, dict) <- get
        case stack of
            [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
            stack -> do
                let last_elem = last stack  -- Получаем последний элемент
                put (stack ++ [last_elem], dict)  -- Добавляем его в конец стека
                return (Right ())

    Drop -> do
        (stack, dict) <- get
        case stack of
            [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
            stack -> do
                put (init stack, dict)  -- Добавляем его в конец стека
                return (Right ())

    Over -> do
        (stack, dict) <- get
        if length stack < 2
            then return (Left StackUnderflow)  -- Ошибка, если элементов меньше 3
            else do
                let secondToLast = last (init stack) -- Предпоследний элемент
                put (stack ++ [secondToLast], dict) -- Переставляем элементы
                return (Right ())

    Rot -> do
        (stack, dict) <- get
        case splitLastThree stack of
            Just (rest, x, y, z) -> do
                put (rest ++ [y, z, x], dict) -- Поменяли местами последние три элемента
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    -- Функции для работы с булевыми и логическими операциями
    Eq -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [if x == y then -1 else 0], dict)
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    Lt -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [if x < y then -1 else 0], dict)
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    Gt -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [if x > y then -1 else 0], dict)
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    And -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [if x /= 0 && y /= 0 then -1 else 0], dict)
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    Or -> do
        (stack, dict) <- get
        case splitLastTwo stack of
            Just (xs, x, y) -> do
                put (xs ++ [if x /= 0 || y /= 0 then -1 else 0], dict)
                return (Right ())
            Nothing -> return (Left StackUnderflow)

    Invert -> do
        (stack, dict) <- get
        case stack of
            [] -> return (Left StackUnderflow)
            xs -> do
                let (initStack, lastElem) = (init xs, last xs)
                put (initStack ++ [if lastElem == 0 then -1 else 0], dict)
                return (Right ())

    -- Функции для ввода/вывода
    PrintTop -> do
        (stack, dict) <- get
        case stack of
            [] -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку
            stack -> do
                let upper_item = last stack
                liftIO (print upper_item) >> return (Right ())  -- Печать верхнего элемента стека
                put (init stack, dict)
                return (Right ())

    Cr -> do
        liftIO (putStrLn "")  -- Печать новой строки
        return (Right ())

    Emit -> do
        (stack, dict) <- get
        case stack of
            [] -> return (Left StackUnderflow)  -- Если стек пуст, ошибка
            stack -> do
                let last_elem = last stack  -- Последний положенный элемент
                if last_elem >= 0 && last_elem <= 255
                    then do
                        liftIO (putChar (toEnum last_elem))  -- Печатаем символ, соответствующий ASCII коду
                        return (Right ())  -- Возвращаем успешный результат
                    else return (Left $ InvalidCommand "Value out of ASCII range")

    PrintString str -> do
        liftIO (putStrLn str)  -- Печать строки
        return (Right ())

    Key -> do
        c <- liftIO getChar  -- Ожидание ввода символа
        modify (\(stack, dict) -> (fromEnum c : stack, dict))  -- Кладем код символа в стек
        return (Right ())

    -- Функция для определения новых слов
    DefineWord word commands -> do
        (stack, dict) <- get
        let newDict = Map.insert word commands dict
        put (stack, newDict)  -- Обновляем словарь
        return (Right ())

    Comment content -> do
        let validation = validateComment (Comment content)
        case validation of
            Left err -> return (Left err)  -- Возвращаем ошибку, если комментарий невалиден
            Right () -> return (Right ())  -- Игнорируем комментарий

    IfElse trueBranch falseBranch -> do
        (stack, dict) <- get
        case stack of
            (x:xs) -> do
                put (xs, dict)  -- Убираем проверяемое значение
                if x /= 0 
                    then mapM_ execute trueBranch  -- Если True, выполняем первую часть
                    else mapM_ execute falseBranch  -- Если False, выполняем вторую часть
                return (Right ())
            _ -> return (Left StackUnderflow)

    Loop commands -> do
        (stack, dict) <- get
        case stack of
            (end:start:xs) -> do
                put (xs, dict)  -- Убираем границы цикла
                loopRun start end commands
                return (Right ())
            _ -> return (Left StackUnderflow)
      where
        loopRun i end cmds
            | i >= end = return ()  -- Выход из цикла
            | otherwise = do
                modify (\(s, d) -> (s ++ [i], d))  -- Кладём i в стек
                mapM_ execute cmds  -- Выполняем тело цикла
                loopRun (i+1) end cmds  -- Увеличиваем i и повторяем

    PrintLiteralString str -> do
        liftIO (putStr (str ++ " "))  -- Выводим строку без перевода строки
        return (Right ())

    execute cmd = case cmd of
    -- ... другие команды ...

    Begin -> do
        (stack, dict) <- get
        put (0 : stack, dict)  -- Добавляем 0 на стек для отслеживания состояния цикла
        return (Right ())

    Until -> do
        (stack, dict) <- get
        case stack of
            (0:rest) -> do
                put (rest, dict)  -- Убираем 0, если условие не выполнено
                return (Right ())
            (1:rest) -> do
                put (rest, dict)  -- Убираем 1, если условие выполнено
                return (Right ())
            _ -> return (Left StackUnderflow)  -- Если стек пустой, возвращаем ошибку

    FtoS -> do
        (stack, dict) <- get
        case stack of
            (Right v:xs) -> put (Left (floor v) : xs, dict) >> return (Right ())  -- Преобразование вещественного в целое
            _ -> return (Left StackUnderflow)

    Leave -> do
        -- Выход из цикла: просто очищаем стек и возвращаемся
        (stack, dict) <- get
        put ([], dict)  -- Можно добавить логику для сохранения состояния, если это необходимо
        return (Right ())

    PlusLoop -> do
        (stack, dict) <- get
        case stack of
            (n:xs) -> do
                let newCount = n + 1
                put (newCount : xs, dict)  -- Увеличиваем счетчик
                return (Right ())
            _ -> return (Left StackUnderflow)
    
    Case branches defaultAction -> do
        (stack, dict) <- get
        case stack of
            (value:xs) -> do
                let (matchedAction, remainingBranches) = findMatchingBranch value branches
                case matchedAction of
                    Just actions -> do
                        put (xs, dict)  -- Убираем значение из стека
                        mapM_ execute actions  -- Выполняем действия
                    Nothing -> do
                        put (defaultAction ++ xs, dict)  -- Выполняем действия по умолчанию
                return (Right ())
            _ -> return (Left StackUnderflow)

    EndCase -> return (Right ())  -- Просто возвращаем, когда встречаем ENDCASE

    EndOf -> return (Right ())  -- Просто возвращаем, когда встречаем ENDOF


-- Проверка валидности комментария
validateComment :: Command -> Either EvalError ()

validateComment (Comment content)
    | "(" `isInfixOf` content || ")" `isInfixOf` content = Left InvalidComment  -- Невалидный, если содержит незакрытые скобки
    | otherwise = Right ()
validateComment _ = Right ()  -- Игнорируем другие команды
