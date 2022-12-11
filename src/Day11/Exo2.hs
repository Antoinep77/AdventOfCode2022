module Day11.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils
import Data.List
import qualified Data.Set as Set

solution = product . take 2 . reverse . sort . fst . curry playGame (replicate 8 0) . const startData

playGame :: ([Int],[[Int]]) -> ([Int],[[Int]])
playGame = foldl1 (.) $ replicate 10000 playRound

playRound :: ([Int],[[Int]]) -> ([Int],[[Int]])
playRound = foldl1 (.) $ map playMonkey $ reverse [0..7] 

playMonkey :: Int -> ([Int],[[Int]]) -> ([Int],[[Int]])
playMonkey index (counts,values) = (newCounts, newEmptiedValues) 
  where 
    newCounts = modifyElement index (counts!!index + count) counts
    newEmptiedValues = modifyElement index [] newValues
    (count,newValues) = playMonkeyWithValues (values!!index) index values

    playMonkeyWithValues :: [Int] -> Int -> [[Int]] -> (Int,[[Int]])
    playMonkeyWithValues [] _ values = (0,values)
    playMonkeyWithValues (val:rest) index values = (\(x,y) -> (x+1,y)) $ playMonkeyWithValues rest index newValues
      where
        score = mod (monkeyOperation index val) globalModulo
        m = nextMonkey index score
        newValues = modifyElement m (score:(values !! m)) values


monkeyOperation :: Int -> Int -> Int 
monkeyOperation 0 = (*7) 
monkeyOperation 1 = (*11) 
monkeyOperation 2 = (+8) 
monkeyOperation 3 = (+7) 
monkeyOperation 4 = (+5) 
monkeyOperation 5 = (+4) 
monkeyOperation 6 = \x -> x*x
monkeyOperation 7 = (+3) 

nextMonkey ::  Int -> Int -> Int
nextMonkey 0 x = if mod x 19 == 0 then 6 else 7
nextMonkey 1 x = if mod x 3 == 0 then 3 else 5
nextMonkey 2 x = if mod x 13 == 0 then 0 else 6
nextMonkey 3 x = if mod x 7 == 0 then 2 else 4
nextMonkey 4 x = if mod x 5 == 0 then 2 else 0
nextMonkey 5 x = if mod x 11 == 0 then 4 else 3
nextMonkey 6 x = if mod x 17 == 0 then 7 else 1
nextMonkey 7 x = if mod x 2 == 0 then 5 else 1

globalModulo = 2 * 3 * 5 * 7 * 11 * 13 *17 * 19

startData = [
    [85, 77, 77],
    [80, 99],
    [74, 60, 74, 63, 86, 92, 80],
    [71, 58, 93, 65, 80, 68, 54, 71],
    [97, 56, 79, 65, 58],
    [77],
    [99, 90, 84, 50],
    [50, 66, 61, 92, 64, 78]
  ]

