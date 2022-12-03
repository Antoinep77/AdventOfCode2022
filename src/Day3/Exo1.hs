module Day3.Exo1
  ( solution,
  )
where

import Utils
import qualified Data.Set as Set
import qualified Data.Char as Char


solution :: String -> Int
solution = sum . map ( charToPriority . uncurry uniqueItem . splitListHalf)  . words

splitListHalf l = splitAt (div (length l) 2) l

uniqueItem :: String -> String -> Char
uniqueItem l1 l2 = head $ Set.toList $ Set.intersection (Set.fromList l1) (Set.fromList l2)

charToPriority :: Char -> Int
charToPriority x | Char.ord x >= 97 = Char.ord x - 97 + 1
                 | otherwise = Char.ord x - 65 + 27


