module Day7.Exo1
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils
import Data.List
import qualified Data.Map as Map

data Tree = Node (Map.Map String Tree) | Leaf Int

instance Show Tree where
    show (Leaf y) = show y
    show (Node m) = show m


solution = count . parseTree (Node Map.empty) [] . split "\n"

parseTree :: Tree -> [String] -> [String] -> Tree
parseTree tree paths [] = tree
parseTree tree paths (h:t) = parseTree newTree newPaths t
  where
    (newTree,newPaths) = applyLine h paths tree 

applyLine :: String -> [String] -> Tree -> (Tree, [String])
applyLine move paths tree
    | move == "$ cd .." = (tree, tail paths)
    | take 4 move == "$ cd" = (tree, (splittedMove!!2):paths)
    | take 4 move == "$ ls" = (tree, paths)
    | take 4 move == "dir " = (tree, paths)
    | otherwise = (addToTree (reverse ((splittedMove!!1):paths)) (Leaf (read ( head splittedMove))) tree, paths) 
  where splittedMove = split " " move


addToTree :: [String] -> Tree -> Tree -> Tree
addToTree [key] val (Node m) = Node $ Map.insert key val m
addToTree (key:t) val (Node m) = Node $ Map.insert key (addToTree t val oldValue) m
  where
    oldValue = getOrElse (Map.lookup key m) $ Node Map.empty

count :: Tree -> Int
count = fst . countWithSize
  where 
  countWithSize (Leaf x ) = (0, x)
  countWithSize (Node m ) | totalSize <= 100000 = (totalValue + totalSize, totalSize)
                          | otherwise = (totalValue, totalSize)
    where
      totalSize = sum $ map (snd . countWithSize) $ Map.elems m
      totalValue = sum $ map (fst . countWithSize) $ Map.elems m
