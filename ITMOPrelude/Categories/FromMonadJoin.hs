{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadJoin where
import ITMOPrelude.Categories.MonadJoin

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

instance MonadJoin m => Monad m where
    return = returnJoin
    (>>=) ma fa = join (fmap fa ma)

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin
    (>=>) f g a = join (fmap g (join (fmap f (returnJoin a)))) 
          