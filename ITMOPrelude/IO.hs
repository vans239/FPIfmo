{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State ( \world -> 
                 let newWorld = RealWorld (tail (stdIn world)) (stdOut world) (exitCode world)
                 in (newWorld, head (stdIn world) ))

putNat :: Nat -> IO ()
putNat n = State ( \world ->
                 let newWorld = RealWorld (stdIn world) (Cons n (stdOut world)) (exitCode world)
                 in (newWorld, ()))

setExitCode :: Nat -> IO ()
setExitCode n = State ( \world ->
                 let newWorld = RealWorld (stdIn world) (stdOut world) n
                 in (newWorld, ()))

