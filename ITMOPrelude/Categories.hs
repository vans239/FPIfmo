{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module ITMOPrelude.Algebra where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import qualified Prelude as P;

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b 
    (>>)   :: m a -> m b -> m b 
    (>>) a b = (>>=) a (\_ -> b)
    return :: a -> m a

class MonadFish m where
    returnFish :: a -> m a
    (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

class Functor m => MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a


--proof
instance Monad m => MonadFish m where
    returnFish = return
    (>=>) fa fb a = ((return a) >>= fa) >>= fb

id x = x
--instance MonadFish m => Monad m where
--    return = returnFish
--    (>>=) ma fa = (>=>) id fa ma  

instance Monad m => Functor m where
    fmap f ma = (>>=) ma (return P.. f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join mma = (>>=) mma id
    
instance MonadJoin m => Monad m where
    return = returnJoin
    (>>=) ma fa = join (fmap fa ma)
-- other
instance Monad List where
    (>>=) list f = concatMap f list
    return a = Cons a Nil
instance Functor List where
    fmap = map