module Day12.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils

import Utils.GraphUtils
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char

solution = (\f (x,y,z) ->  f x y z) dijkstraMultipleEnds . parseData

parseData:: String -> (Map.Map (Int,Int) [(Int,Int)], (Int,Int), Set.Set (Int,Int))
parseData input = (adjacencyMap, start, ends)
  where
    start = findCharCoord 'E' $ concat allLines
    ends = findAllCharCoord 'a' $ concat allLines
    adjacencyMap = foldl1 (Map.unionWith (++)) $ map lineToAdjacencyMap allLines
    allLines = allLinesIteratorWithReverse $ split "\n" input

lineToAdjacencyMap :: [(Char,Int,Int)] -> Map.Map  (Int,Int) [(Int,Int)]
lineToAdjacencyMap [x] = Map.empty
lineToAdjacencyMap ((x,i,j):(y,k,l):rest) | (x=='S' && ord y <= ord 'a' + 1) 
                                            || (y =='E' && ord 'z' <= ord x + 1) 
                                            ||(x/='S' && y /='E' && ord y <= ord x + 1)
                                            = Map.insertWith (++) (k,l) [(i,j)] $ lineToAdjacencyMap $ (y,k,l):rest
                                          | otherwise = lineToAdjacencyMap ((y,k,l):rest)


findCharCoord :: Char -> [(Char,Int,Int)] -> (Int,Int)
findCharCoord c ((x,i,j):rest) | c==x = (i,j)
                                | otherwise = findCharCoord c rest

findAllCharCoord :: Char -> [(Char,Int,Int)] -> Set.Set (Int,Int)
findAllCharCoord _ [] = Set.empty
findAllCharCoord c ((x,i,j):rest) | c==x = Set.insert (i,j) $  findAllCharCoord c rest
                               | otherwise = findAllCharCoord c rest

