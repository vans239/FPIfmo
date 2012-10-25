{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List

class Monoid a where
   mempty :: a
   mappend :: a -> a -> a
   mconcat :: List a -> a
   mconcat Nil = mempty
   mconcat (Cons a list) = mappend a (mconcat list)

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

class Group a where
    gempty :: a
    gappend :: a -> a -> a
    ginv :: a -> a


