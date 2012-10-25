{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Из этих
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем эти

instance MonadJoin m => MonadFish m where
    returnFish = ?
    f >=> g = ?

instance Monad m => MonadFish m where
    returnFish = return
    (>=>) fa fb a = ((return a) >>= fa) >>= fb

