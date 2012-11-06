-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like, aEof
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a) 
    ma >>= f = let applyFunc a list = case (runParser (f a) list) of
                                        Right (list2, b) -> Right (list2, b)   
                                        otherwise -> Left ParseError
               in 
                 let checkErrors xs = case runParser ma xs of
                                  Right (list, a) -> applyFunc a list
                                  otherwise -> Left ParseError
                 in Monstupar (\xs -> checkErrors xs)  
 
--ma (a -> mb) mb
--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = return ()

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left ParseError

-- Конец ввода
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left ParseError

aEof :: a -> Monstupar s a
aEof a = Monstupar $ \s -> case s of
    [] -> Right (s , a)
    _  -> Left ParseError
infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
                              Left _ -> runParser b s
                              success -> success

-- В голове ввода сейчас нечто, удовлетворяющее p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \xs -> 
         let apply (x:xs) = if (p x) 
                           then Right (xs, x)
                           else Left ParseError
         in case xs of
              [] -> Left ParseError
              xs -> apply xs

-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

