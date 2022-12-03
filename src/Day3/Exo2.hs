module Day3.Exo2
  ( solution,
  )
where

import qualified Data.Set as Set
import qualified Data.Char as Char


solution :: String -> Int
solution = sum . map ( charToPriority . (\ f [x,y,z] -> f x y z) findBadge) . groupItems 3 . words

groupItems :: Int -> [a] -> [[a]]
groupItems n [] = []
groupItems n l = first:groupItems n rest
  where (first,rest) = splitAt n l


findBadge :: String -> String -> String -> Char
findBadge l1 l2 l3 = head $ Set.toList $ Set.intersection (Set.fromList l1) $ Set.intersection (Set.fromList l2) (Set.fromList l3)

charToPriority :: Char -> Int
charToPriority x | Char.ord x >= 97 = Char.ord x - 97 + 1
                   | otherwise = Char.ord x - 65 + 27

