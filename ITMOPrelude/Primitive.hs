{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)
import qualified Prelude as P;

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLe --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

boolCmp :: Bool -> Bool -> Bool
boolCmp True True = True
boolCmp False False = True
boolCmp a b = False

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp a b = if' (natEq a b) EQ (if' (natLt a b) LT GT) 

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

natGt :: Nat -> Nat -> Bool
natGt a b = case (natCmp a b) of 
      GT -> True
      otherwise -> False 

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. m = Zero
n -. Zero = n
(Succ n) -. (Succ m) = n -. m 

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = if' (natGt m n) (Pair Zero n) 
          (Pair 
          (fst (natDivMod (n -. m) m) +. natOne) 
          (snd (natDivMod (n -. m) m)))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd a b = (if' (natEq b Zero) a (if' (natLt a b) (gcd b a) (gcd (natMod a b) b))) 

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Natural Nat | NegMinusOne Nat  deriving (Read)

intZero   = Natural natZero   -- 0
intOne    = Natural natOne     -- 1
intNegOne = NegMinusOne natZero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg a@(Natural Zero) = a
intNeg (Natural (Succ n)) = NegMinusOne (n)
intNeg (NegMinusOne n) = Natural (n +. natOne)

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp a b =  if' (intEq a b) EQ (if' (intLt a b) LT GT)

intEq :: Int -> Int -> Bool
intEq (Natural n) (Natural m) = (natEq n m)
intEq (NegMinusOne n) (NegMinusOne m) = (natEq n m)
intEq n m = False

intLt :: Int -> Int -> Bool
intLt (Natural n) (Natural m) = (natLt n m)
intLt (NegMinusOne n) (NegMinusOne m) = (natLt m n)
intLt (NegMinusOne n) m = True
intLt n m = False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Natural n) .+. (Natural m) = Natural (n +. m)
(NegMinusOne n) .+. (NegMinusOne m) = NegMinusOne (n +. m +. natOne)
(Natural n) .+. (NegMinusOne m) = if' (natLt n (m +. natOne)) 
         (NegMinusOne (m -. n)) 
         (Natural (n -. m -. natOne))
         
n .+. m = m .+. n
 
(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
Natural Zero .*. m = Natural Zero
NegMinusOne Zero .*. m = intNeg m 
NegMinusOne n .*. m = (NegMinusOne (n -. natOne)) .*. m  .-. m
Natural n .*. m = (Natural (n -. natOne)) .*. m .+. m

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat  deriving (Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (NegMinusOne a) b) = Rat (intNeg (Natural b)) (a +. natOne)
ratInv (Rat (Natural a) b) = Rat (Natural b) a

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp a b =  if' (ratEq a b) EQ (if' (ratLt a b) LT GT)

ratEq :: Rat -> Rat -> Bool
ratEq (Rat a b) (Rat c d) = if' (intEq (a .*. (Natural d)) (c .*. (Natural b))) True False

ratLt :: Rat -> Rat -> Bool
ratLt (Rat a b) (Rat c d) = if' (intLt (a .*. (Natural d)) (c .*. (Natural b))) True False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a b) %+ (Rat c d) = ratNorm (Rat ((a .*. (Natural d)) .+. (c .*. (Natural b))) (b *. d))

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) = ratNorm (Rat (a .*. c) (b *. d)) 

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

ratNorm :: Rat -> Rat
ratNorm (Rat (Natural a) b) = Rat (Natural (natDiv a (gcd a b))) (natDiv b (gcd a b))
ratNorm a = ratNeg (ratNorm (ratNeg a))
-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b

asNat :: P.Integer -> Nat
asNat 0 = Zero
asNat n = Succ (asNat (n P.- 1)) 

asInt :: P.Integer -> Int
asInt n | (n P.>= 0) = Natural (asNat n)
      | (n P.< 0) = NegMinusOne ( asNat((0 P.-1) P.- n)) 

asPInt :: Nat -> P.Integer
asPInt Zero = 0
asPInt (Succ n) = (asPInt n) P.+ 1

iasPInt :: Int -> P.Integer
iasPInt (Natural n) = asPInt n
iasPInt n@(NegMinusOne _) = iasPInt (intNeg n)
 
instance Show Nat where 
         show n = P.show (asPInt n)

instance Show Int where 
         show n = P.show (iasPInt n)

instance Show Rat where 
         show (Rat a b) = P.show (iasPInt a) P.++ "/" P.++ P.show (asPInt b)
