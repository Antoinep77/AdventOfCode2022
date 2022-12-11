module Day7.Exo2
  ( solution,
  )
where

import Utils
import Data.List
import qualified Data.Map as Map

data Tree = Node (Map.Map String Tree) | Leaf Int

instance Show Tree where
    show (Leaf y) = show y
    show (Node m) = show m


solution = minDirSize . parseTree (Node Map.empty) [] . split "\n"

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

getTotalSize :: Tree -> Int
getTotalSize (Leaf x) = x
getTotalSize (Node m) = sum $ map getTotalSize $ Map.elems m

minDirSize :: Tree -> Int
minDirSize tree = minimum $ filter (\x -> x >= 30000000 - 70000000 + getTotalSize tree) $ snd $  sizes tree
  where
    sizes ::  Tree -> (Int,[Int])
    sizes (Leaf x ) = (x,[x])
    sizes (Node m ) = ( dirSize , dirSize:concatMap snd recValues )
      where
        dirSize = sum $ map fst recValues
        recValues = map sizes $ Map.elems m
