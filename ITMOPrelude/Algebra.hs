{-# LANGUAGE NoImplicitPrelude #-}
<<<<<<< HEAD
module ITMOPrelude.Algebra where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

class Monoid a where
   mempty :: a
   mappend :: a -> a -> a
   mconcat :: List a -> a
   mconcat Nil = mempty
   mconcat (Cons a list) = mappend a (mconcat list)

class Monoid a => Group a where
    ginv :: a -> a

--concat
instance Monoid (List a) where
   mempty = Nil
   mappend = (++)

-- +
instance Monoid Nat where
   mempty = Zero
   mappend = (+.)

-- max
minusInfinity = minusInfinity .-. intOne
instance Monoid Int where
   mempty = minusInfinity
   mappend a b = case (intCmp a b) of
        LT -> b
        otherwise -> a 

-- *
instance Monoid Rat where
   mempty = Rat intOne natOne
   mappend = (%*)   


=======
module ITMOPredule.Algebra where

import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

