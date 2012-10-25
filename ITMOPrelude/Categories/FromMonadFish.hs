{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadFish where
import ITMOPrelude.Categories.MonadFish

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin
import qualified Prelude as P;

empty x = x
instance MonadFish m => Monad m where
   return = returnFish
   (>>=) ma fa = (>=>) empty fa ma
          
instance MonadFish m => Functor m where
    fmap f ma = (>=>) empty (monoidF f) ma
         where monoidF f = returnFish P.. f

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join mma = (>=>) empty empty mma

-- m(m a) -> m a  == (>=>) :: (m m a -> m (m a)) -> (m a -> m (a)) -> m a
-- default
--class MonadFish m where
--    returnFish :: a -> m a
--    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c) 

--class Functor m => MonadJoin m where
--    returnJoin :: a -> m a
--    join :: m (m a) -> m a

--class Functor f where
--    fmap :: (a -> b) -> f a -> f b

--class Monad m where
--    (>>=)  :: m a -> (a -> m b) -> m b 
--    return :: a -> m a




