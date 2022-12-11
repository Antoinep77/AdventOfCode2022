module Day1.Exo1
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils
import Data.List

solution1 :: String -> Int
solution1 = maximum . map (sum .  map read . words ) . split "\n\n"


solution :: String -> Int
solution = sum .take 3 . reverse . sort . map (sum . map read . words ) . split "\n\n"