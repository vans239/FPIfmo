{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import qualified Prelude as P;

data Tree a = NilTree | ConsTree a (Tree a) (Tree a) deriving (Read, Show)

createEmpty :: Tree a
createEmpty = NilTree

addToTop :: a -> Tree a -> Tree a
addToTop a tree = ConsTree a tree NilTree

addToLeft :: a -> Tree a -> Tree a
addToLeft a NilTree = ConsTree a NilTree NilTree
addToLeft a (ConsTree child left right) = ConsTree child (addToLeft a left) right

addToRight :: a -> Tree a -> Tree a
addToRight a NilTree = ConsTree a NilTree NilTree
addToRight a (ConsTree child left right) = ConsTree child left (addToRight a right)

rotateRight :: Tree a -> Tree a
rotateRight (ConsTree a (ConsTree b cLeft cRight) right) = ConsTree b cLeft 
            (ConsTree a cRight right)

rotateLeft :: Tree a -> Tree a
rotateLeft (ConsTree a left (ConsTree b cLeft cRight)) = ConsTree b 
           (ConsTree a left cLeft) cRight

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree fun NilTree = NilTree
mapTree fun (ConsTree a left right) = ConsTree (fun a) (mapTree fun left) (mapTree fun right)
   
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree fun b NilTree = b
foldrTree fun b (ConsTree a left right) = foldrTree fun (fun a (foldrTree fun b right)) left

