
module Day8.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils as ListUtils
import Data.List
import qualified Data.Set as Set

solution = solve . parseGrid

parseGrid :: String -> [[Int]]
parseGrid =  map (map (read . pure)) . split "\n"

scenicScore :: Int -> [[Int]] -> Int
scenicScore maxHeigth = product . map (getLineCountsWithMaxHeight maxHeigth)


getLineCountsWithMaxHeight :: Int -> [Int] -> Int
getLineCountsWithMaxHeight _ [] = 0
getLineCountsWithMaxHeight maxHeight (x:rest)
                              | x<maxHeight  = 1 + getLineCountsWithMaxHeight maxHeight rest
                              | otherwise = 1
  

getScenicScorByCoord :: [[Int]] ->  (Int,Int) -> Int
getScenicScorByCoord grid (x,y)  = scenicScore height [leftView,rightView,topView,bottomView]
  where 
    height = grid !! x !! y 
    leftView = reverse $ take y (grid !! x) 
    rightView = drop (y+1) (grid !! x) 
    topView = reverse $ take x (transposedGrid !! y) 
    bottomView = drop (x+1) (transposedGrid !! y)
    transposedGrid = ListUtils.transpose grid

solve :: [[Int]] -> Int
solve grid = maximum $ map (getScenicScorByCoord grid) $ enumerate [0..(length grid - 1)] [0..(length (head grid) - 1)]