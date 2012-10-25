{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

instance Monad m => Functor m where
    fmap f ma = (>>=) ma (return P.. f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join mma = (>>=) mma id
