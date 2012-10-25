{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonad where
import ITMOPrelude.Categories

import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish
import qualified Prelude as P;

instance Monad m => Functor m where
    fmap f ma = (>>=) ma (return P.. f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join mma = (>>=) mma id
         where id x = x 

instance Monad m => MonadFish m where
    returnFish = return
    (>=>) fa fb a = ((return a) >>= fa) >>= fb

