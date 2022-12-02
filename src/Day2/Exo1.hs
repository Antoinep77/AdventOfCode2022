module Day2.Exo1
  ( solution,
  )
where

import Utils

solution :: String -> Int
solution = sum . map ((\[x,y] -> score x y ) . words ) . split "\n"


scoreForSign :: String -> Int
scoreForSign "X" = 1
scoreForSign "Y" = 2
scoreForSign "Z" = 3

transformSign :: String -> String
transformSign "X" = "A"
transformSign "Y" = "B"
transformSign "Z" = "C"

scoreForOutcome :: String -> String -> Int
scoreForOutcome "A" "B"  = 6
scoreForOutcome "B" "C"  = 6
scoreForOutcome "C" "A"  = 6
scoreForOutcome x y  
  | x == y = 3
  | otherwise = 0



score :: String -> String -> Int
score x y = scoreForSign y + scoreForOutcome x (transformSign y)

