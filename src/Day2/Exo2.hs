module Day2.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils

solution :: String -> Int
solution = sum . map ((\[x,y] -> score x y ) . words ) . split "\n"


scoreForSign :: String -> Int
scoreForSign "A" = 1
scoreForSign "B" = 2
scoreForSign "C" = 3

getSignFromOutcome :: String -> String -> String
getSignFromOutcome x "Y" = x
getSignFromOutcome "B" "X" = "A"
getSignFromOutcome "C" "Z" = "A"
getSignFromOutcome "C" "X" = "B"
getSignFromOutcome "A" "Z" = "B"
getSignFromOutcome _ _ = "C"


scoreForOutcome :: String -> Int
scoreForOutcome "X"  = 0
scoreForOutcome "Y"  = 3
scoreForOutcome "Z"  = 6


score :: String -> String -> Int
score x y = scoreForSign (getSignFromOutcome x y) + scoreForOutcome y

