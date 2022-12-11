module Day9.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.VectorUtils
import Data.List
import qualified Data.Set as Set

solution = length . setOfPositions  (replicate 10 [0,0]) . parseMoves

parseMoves :: String -> [String]
parseMoves = concatMap (\[m,count]-> replicate (read count) m ) . map words . split "\n"

setOfPositions :: [[Int]] -> [String] ->  Set.Set [Int]
setOfPositions rope [] = Set.singleton $ last rope
setOfPositions rope (move:rest) = Set.insert (last rope) $ setOfPositions newWholeRope rest
  where
    newWholeRope = moveWholeRope rope move

moveWholeRope :: [[Int]] -> String -> [[Int]]
moveWholeRope (h:t) move = newHead : moveWholeRopeWithoutHead newHead t
  where
    newHead = moveHead move h
    moveWholeRopeWithoutHead :: [Int] -> [[Int]] -> [[Int]]
    moveWholeRopeWithoutHead _ [] = []
    moveWholeRopeWithoutHead prev (rope:rest) = newRope:moveWholeRopeWithoutHead newRope rest
      where
        newRope = moveTail prev rope

moveHead :: String -> [Int] -> [Int]
moveHead "U" [x,y] = [x-1,y]
moveHead "D" [x,y] = [x+1,y]
moveHead "L" [x,y] = [x,y-1]
moveHead "R" [x,y] = [x,y+1]


moveTail :: [Int] -> [Int] -> [Int]
moveTail headCoord tailCoord | manDist <= 1 = tailCoord
                                 | manDist == 2 && (xtail == xhead || ytail == yhead)
                                          =  [xtail + div (xhead - xtail) 2, ytail + div (yhead - ytail) 2]
                                 | manDist == 2 = tailCoord
                                 | otherwise = [xtail + signum (xhead - xtail), ytail + signum (yhead - ytail)]
  where
    [xtail,ytail] = tailCoord
    [xhead,yhead] = headCoord
    manDist =manathan headCoord tailCoord

