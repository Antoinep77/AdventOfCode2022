module Day6.Exo1
  ( solution,
  )
where

import Utils
import Data.List
import qualified Data.Set as Set

solution1 :: String -> Int
solution1 (a:b:c:d:rest) | areDistinct [a,b,c,d] = 4
                        | otherwise = 1 + solution (b:c:d:rest)

solution :: String -> Int
solution list | areDistinct (take 14 list) = 14
              | otherwise = 1 + solution (tail list)

areDistinct list = length list == length (Set.fromList list)