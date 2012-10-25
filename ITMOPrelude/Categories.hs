{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b 
    (>>)   :: m a -> m b -> m b 
    (>>) a b = (>>=) a (\_ -> b)
    return :: a -> m a

-- other
instance Monad List where
    (>>=) list f = concatMap f list
    return a = Cons a Nil
instance Functor List where
    fmap = map

