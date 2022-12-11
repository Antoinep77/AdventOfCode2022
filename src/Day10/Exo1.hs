module Day10.Exo1
  ( solution,
  )
where

import Utils.Utils
import Utils.VectorUtils
import Data.List
import qualified Data.Set as Set

solution = signalStrength [20,60,100,140,180,220] . applyInstructions . concatMap parseInstruction . split "\n"

parseInstruction :: String -> [Int]
parseInstruction "noop" = [0]
parseInstruction str = [0,read $ last $ words str]


applyInstructions :: [Int] -> [Int]
applyInstructions = applyInstructionsWithX 1
  where
    applyInstructionsWithX :: Int -> [Int] -> [Int]
    applyInstructionsWithX _ [] = []
    applyInstructionsWithX currentX (value:rest)
      = currentX : applyInstructionsWithX (currentX + value) rest

signalStrength :: [Int] -> [Int] -> Int
signalStrength indexes xValues = sum $ map (\k -> (xValues !! (k-1)) * k) indexes