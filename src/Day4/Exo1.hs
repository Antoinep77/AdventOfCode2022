module Day4.Exo1
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils
import qualified Data.Tuple as Tuple


solution1 :: String -> Int
solution1 = length . filter (uncurry isContained) . map ((\[x,y] -> (x,y)) . map ( map read . split "-") . split ",")  . split "\n"


solution :: String -> Int
solution = length . filter (uncurry overlaps) . map ((\[x,y] -> (x,y)) . map ( map read . split "-") . split ",")  . split "\n"


isContained :: [Int] -> [Int] -> Bool
isContained [x1,y1] [x2,y2] = (x1 <= x2 && y1 >= y2) || (x1 >= x2 && y1 <= y2)

overlaps :: [Int] -> [Int] -> Bool
overlaps [x1,y1] [x2,y2] =  y1 >= x2 && y2 >= x1





