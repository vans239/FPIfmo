
module OrgParser
   where

import Monstupar.Core
import Monstupar.Derived

tag :: Monstupar Char [Char]
tag =  ( do
         many1 capitalLetter
        ) 

tags :: Monstupar Char [Tag]
tags = ( do
         separated tag space
       )

star = char '*'
stars = many1 star

dataLine :: Monstupar Char Leaf
dataLine = ( do
             _stars <- stars
             _tags <- optional ( do
                 space
                 tags
               )
             space
             _data <- many1 (notChar '\n')
             char '\n'
             return (Leaf (length _stars) (emptyIfNull _tags) _data)
           )

type Tag = String
type Data = String
--level, tags, data
data Leaf = Leaf { getLevel :: Int, getTags :: [Tag], getData :: Data } deriving (Read) 
-- data, childs
data Tree = Tree { getLeaf :: Leaf, getChilds :: [Tree] } deriving (Read) 

instance Show Leaf where
    show leaf = (take (getLevel leaf) (repeat '*')) ++ " [Tags] " ++ concat(getTags leaf) ++ " [Data] " ++ (getData leaf) ++ "\n"
instance Show Tree where
    show tree = show (getLeaf tree) ++ show (getChilds tree) ++ "\n"

headLeaf :: Leaf
headLeaf = Leaf 0 [] ""

headTree :: Tree
headTree = Tree headLeaf []

--parent -> new parent
parser :: Tree -> Monstupar Char [Leaf]
parser parent = ( do 
                  many1 dataLine
                )

main = ( do
         fileData <- readFile "test.org"
         --putStr (show (testFind "TODO" fileData))
         putStr (show (runParser (buildTreeMonstupar (parser headTree)) fileData))
         return ()
          )

testOrgParser s = runParser (parser headTree) s

test1 = testOrgParser "* TODO 123324\n"
test2 = testOrgParser "** c\n"
test3 = testOrgParser "* TODO 123324\n** c\n* a\n** b\n** d\n"

findMonstupar :: Monstupar Char [Leaf] -> Tag -> Monstupar Char [Leaf]
findMonstupar parser tagValue = parser >>= (\list -> return (find list tagValue))
                   
find :: [Leaf] -> Tag -> [Leaf]
find [] tag = []
find (e : es) tag = if elem tag (getTags e) 
                    then (e : fits ++ (find unfits tag))
                    else find es tag
    where
      (fits, unfits) = break (\x -> (getLevel e) < (getLevel x)) es

testFind tag = runParser (findMonstupar (parser headTree) tag)
testFind1 = testFind "TODO" "* TODO 123324\n** c\n* a\n** b\n** d\n"

buildTreeMonstupar :: Monstupar Char [Leaf] -> Monstupar Char [Tree]
buildTreeMonstupar parser = parser >>= (\list -> return (buildTree list))

buildTree :: [Leaf] -> [Tree]
buildTree [] = []
buildTree (e : es) = ( Tree e (buildTree fits) : buildTree unfits)
    where
      (fits, unfits) = break (\x -> (getLevel e) > (getLevel x)) es