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