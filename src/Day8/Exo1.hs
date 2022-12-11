module Day8.Exo1
  ( solution,
  )
where

import Utils
import Data.List
import qualified Data.Set as Set

solution = length . getAllIndexes . parseGrid

parseGrid :: String -> [[Int]]
parseGrid =  map (map (read . pure)) . split "\n"

getLineIndexes :: [Int] -> Set.Set Int
getLineIndexes = getLineIndexesWithMaxHeight (-1)
  where
    getLineIndexesWithMaxHeight _ [] = Set.empty
    getLineIndexesWithMaxHeight maxHeight (x:rest)
                                  | x>maxHeight  = Set.insert 0 $ Set.map (+1) (getLineIndexesWithMaxHeight x rest )
                                  | otherwise = Set.map (+1) $ getLineIndexesWithMaxHeight maxHeight rest
  

getGridIndexes :: [[Int]] -> Set.Set (Int, Int)
getGridIndexes  [] = Set.empty
getGridIndexes  (line:rest) = Set.union lineIndexes recIndexes
  where lineIndexes = Set.map (\x -> (0,x)) $ getLineIndexes line
        recIndexes = Set.map (\(x,y) -> (x+1,y)) $ getGridIndexes rest

getReverseGridIndex grid = Set.map (\(x,y) -> (x, length (head grid) - 1 - y)) $ getGridIndexes reversedGrid
  where reversedGrid = map reverse grid

getTransposedGridIndex grid = Set.map (\(x,y) -> (y,x)) $ getGridIndexes $ Utils.transpose grid

getReversedTransposedGridIndex grid = Set.map (\(x,y) -> (length (head grid) - 1 - y, x)) $
                               getGridIndexes $ map reverse $ Utils.transpose grid


getAllIndexes grid = Set.union ( getTransposedGridIndex grid) $
              Set.union  ( getReversedTransposedGridIndex grid) $ 
              Set.union  ( getReverseGridIndex grid) $
              getGridIndexes grid