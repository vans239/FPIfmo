module Monstupar.Derived where

-- В этом модуле не видно структуры типа Monstupar из
import Monstupar.Core
-- например,
-- blah = Monstupar $ undefined
-- не скомпилируется, поскольку конструктор Monstupar приватный,
-- поэтому конструировать парсеры тут можно только используя примитивные
-- парсеры из Core.

--------------------------------------------------------------------------------
-- Всякие удобные и полезные штуки

-- Всё плохо
notok :: Monstupar s ()
notok = isnot ok

-- В голове ввода сейчас в точности s
char :: Eq s => s -> Monstupar s s
char = like . (==)

-- В голове ввода сейчас что-то из списка
oneOf :: Eq s => [s] -> Monstupar s s
oneOf [] = like (\x -> False)
oneOf (x:xs) = char x <|> oneOf xs

-- В префиксе головы сейчас нечто вполне определённое
string :: Eq s => [s] -> Monstupar s [s]
string [] = return []
string (x:xs) = do  
  e <- char x
  es <- string xs
  return (e:es)
                
-- "Звёздочка" -- запустить парсер максимальное (ноль или более) число раз и
-- саккумулировать результыты
many :: Monstupar s a -> Monstupar s [a]
many p = many1 p <|> return []
-- Аккуратно с реализацией! Следите за тем, чтобы у вас из-за использования <|>
-- не рос в бесконечность стек.

-- "Плюсик" -- один или более раз
many1 :: Monstupar s a -> Monstupar s [a]
many1 p = do
    e <- p
    es <- many p
    return (e:es)

-- "Вопросик" -- ноль или один раз
optional :: Monstupar s a -> Monstupar s (Maybe a)
optional p = (p >>= (\a -> return (Just a))) <|> return Nothing

digit :: Monstupar Char Char
digit = like (\x -> x >= '0' && x <= '9')

notChar :: Eq s => s -> Monstupar s s
notChar = like . (/=)

notOneOf :: Eq s => [s] -> Monstupar s s
notOneOf [] = like (\x -> True)
notOneOf (x:xs) = (notChar x >> (return ())) <&> notOneOf xs


capitalLetter :: Monstupar Char Char
capitalLetter = like (\x -> x >= 'A' && x <= 'Z')

lowerLetter :: Monstupar Char Char
lowerLetter = like (\x -> x >= 'a' && x <= 'z')

letter = lowerLetter <|> capitalLetter

space :: Monstupar Char [Char]
space = many1 (char ' ')

separated :: Monstupar Char a -> Monstupar Char b -> Monstupar Char [a]
separated a separator  = (do 
                           e <- a
                           es <- many ( (do 
                                     separator 
                                     a
                                   )
                                 )
                           return (e : es)
                         ) <|> return []

emptyIfNull :: Maybe [a] -> [a]
emptyIfNull Nothing = []
emptyIfNull (Just arr) = arr


