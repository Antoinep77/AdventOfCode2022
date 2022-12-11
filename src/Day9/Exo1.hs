module Day9.Exo1
  ( solution,
  )
where

import Utils.Utils
import Utils.VectorUtils
import Data.List
import qualified Data.Set as Set

solution = length . setOfPositions [0,0] [0,0] . parseMoves

parseMoves :: String -> [String]
parseMoves = concatMap (\[m,count]-> replicate (read count) m ) . map words . split "\n"

setOfPositions :: [Int] -> [Int] -> [String] ->  Set.Set [Int]
setOfPositions _ tailCoord [] = Set.singleton tailCoord
setOfPositions headCoord tailCoord (move:rest) = Set.insert tailCoord $ setOfPositions newHead newTail rest
  where
    newTail = moveTail newHead tailCoord
    newHead = moveHead move headCoord


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

