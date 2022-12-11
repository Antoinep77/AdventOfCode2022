module Day5.Exo2
  ( solution,
  )
where

import Utils.Utils
import Utils.ListUtils
import qualified Data.Tuple as Tuple


solution = (\[x,y] -> solve x y)  . split "\n\n"


parseQueues :: String -> [[Char]]
parseQueues str = map reverse $ parseQueuesWithAcc strList $ replicate stackNumber []
  where 
    stackNumber = div (length (head strList)) 4 + 1
    strList = reverse $ drop 1 $ reverse $ split "\n" str
    parseQueuesWithAcc :: [String] -> [[Char]] ->  [[Char]]
    parseQueuesWithAcc [] acc = acc
    parseQueuesWithAcc (str:rest) acc = parseQueuesWithAcc rest (addModulosToList 4 1 str acc)

    addModulosToList :: Int -> Int -> [Char] -> [[Char]] ->  [[Char]]
    addModulosToList _ _ _ [] = []
    addModulosToList n 0 (c:rest) (h:t) 
      | c == ' ' = h : addModulosToList n (n-1) rest t
      | otherwise = (c:h) : addModulosToList n (n-1) rest t
    addModulosToList n k (c:rest) acc = addModulosToList n (k-1) rest acc

applyMoveToColumns :: String -> String -> Int -> (String,String)
applyMoveToColumns receiving giving n = applyMoveToColumnsWithAcc receiving giving n []
  where
    applyMoveToColumnsWithAcc receiving giving 0 acc = (reverse acc ++ receiving, giving)
    applyMoveToColumnsWithAcc receiving (h:t) n acc = applyMoveToColumnsWithAcc receiving t (n-1) (h:acc)

applyMove :: [String] -> (Int,Int,Int) -> [String]
applyMove list (x,y,z) = modifyElement (y-1) newGiving $ modifyElement (z-1) newReceiving list
  where
    (newReceiving,newGiving) = applyMoveToColumns (list!!(z-1)) (list!!(y-1)) x

parseMove :: String -> (Int,Int,Int)
parseMove str =  (read x, read y, read z)
  where [_,x,_,y,_,z] = words str

result :: [String] -> [(Int,Int,Int)] -> String
result queues [] = map head queues
result queues (h:t) = result (applyMove queues h) t 

solve queuesStr movesStr = result queues moves
  where 
    queues = parseQueues queuesStr
    moves = map parseMove $ split "\n" movesStr

