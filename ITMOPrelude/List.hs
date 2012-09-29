{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import qualified Prelude as P;
---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving ( Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons m n ) = length(n) +. Succ Zero 

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons m n) ++ b = Cons m (n ++ b)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = Nil
tail (Cons m n) = n

-- Список без последнего элемента
init :: List a -> List a
init (Cons m Nil) = Nil 
init (Cons m n) = Cons m (init n)

-- Первый элемент
head :: List a -> a
head (Cons m n) = m

-- Последний элемент
last :: List a -> a
last (Cons m Nil) = m
last (Cons m n) = last n

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero s = Nil
take a s = Cons (head s) (take (a -. natOne) (tail s))

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero s = s
drop a s = drop (a -. natOne) (tail s) 

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter fun Nil = Nil
filter fun (Cons s xs) = if' (fun s) (Cons s (filter fun xs)) (filter fun xs)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter fun Nil = Nil
gfilter fun (Cons s xs) = case (fun s)  of
        Nothing -> (gfilter fun xs) 
        otherwise -> (Cons (value (fun s)) (gfilter fun xs)) 
        where
        value (Just a) = a

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile fun Nil = Nil
takeWhile fun (Cons s xs) = if' (fun s) (Cons s (takeWhile fun xs)) (Nil)

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile fun all@(Cons s xs) = if' (fun s) (dropWhile fun xs) all

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span fun s = Pair (takeWhile fun s) (dropWhile fun s)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break a s = myBreak a s Nil

myBreak a Nil t = Pair t Nil
myBreak a (Cons s xs) t = if' (a s) (myBreak a xs (Cons s t)) (Pair t (Cons s xs))

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! n = xs !! (n -. natOne)

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse all@(Cons s Nil) = all 
reverse (Cons s xs) = (reverse xs) ++ (Cons s Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = subsequences(xs) ++ (map mapper (subsequences(xs)))
             where 
             mapper s = Cons x s       
indexes :: Nat -> List Nat
indexes Zero = Nil
indexes n = indexes (n -. natOne) ++ (Cons (n -. natOne) Nil)

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations s = concatMap mapper (indexes (length s))
             where
             mapper a = myPermutations s a 

myPermutations :: List a -> Nat -> List (List a)
myPermutations s n = map mapper (permutations (getOtherString s n))
             where 
             mapper a = (Cons (s !! n) a)    

getOtherString s n = (take n s) ++ (drop (n +. natOne) s)
                        
             
-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a list where list = Cons a list

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f a Nil = a
foldl f a (Cons s xs) = foldl f (f a s) xs 

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f a Nil = (Cons a Nil)
scanl f a (Cons s xs) = Cons a (scanl f (f a s) xs)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b Nil = b
foldr f b (Cons s xs) = f s (foldr f b xs) 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f b Nil = (Cons b Nil)
scanr f b (Cons s xs) = Cons (f s (head other)) other
      where
      other = scanr f b xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f l = gfilter mapper l
    where 
    mapper a = Just (f a)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat a = concatMap mapper a
    where 
    mapper a = a

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons s xs) = (f s) ++ (concatMap f xs) 

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a b = zipWith pairFunc a b
pairFunc a b = Pair a b 

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith func Nil b = Nil
zipWith func a Nil = Nil
zipWith func (Cons a n) (Cons b m) = Cons (func a b) (zipWith func n m) 
     
asList :: [P.Char] -> List P.Char
asList [] = Nil
asList (s : xs) = Cons s (asList xs)    

asString :: List P.Char -> [P.Char]
asString Nil = []
asString (Cons s xs) = (s : (asString  xs))


instance Show a => Show (List a) where
         show (Nil) = "" 
         show (Cons x xs) = P.show x P.++ P.show xs