{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Из этих
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем эту
id x = x
instance MonadFish m => Monad m where
    return = returnFish
    (>>=) ma fa = (>=>) id fa ma

instance MonadJoin m => Monad m where
    return = returnJoin
    (>>=) ma fa = join (fmap fa ma)
