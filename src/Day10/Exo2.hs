module Day10.Exo2
  ( solution,
  )
where

import Utils.Utils
import Data.List
import qualified Data.Set as Set

solution = message . applyInstructions . concatMap parseInstruction . split "\n"

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

getLetter index x = if abs (mod index 40 - x) <= 1 then "#" else "."

addNewLine index = if mod index 40 == 39 && index /= 0 then "\n" else ""

toText index x = getLetter index x ++ addNewLine index

message :: [Int] -> String
message = intercalate "" . zipWith toText [0..]

