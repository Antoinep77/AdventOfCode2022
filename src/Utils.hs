module Utils
  ( split,
  )
where

split :: String -> String -> [String]
split sep str = filter (/= "") (splitWithAcc "" sep str)
  where
    splitWithAcc :: String -> String -> String -> [String]
    splitWithAcc acc sep "" = [reverse acc]
    splitWithAcc acc sep str
      | startWith sep str = reverse acc : splitWithAcc "" sep (removeNFirst (length sep) str)
      | otherwise =  splitWithAcc (head str : acc) sep (tail str)
  

startWith :: String -> String -> Bool
startWith "" _ = True
startWith _ "" = False
startWith (h:t) (h2:t2) = h == h2 && startWith t t2


removeNFirst :: Int -> String -> String
removeNFirst n = foldl1 (.) $ replicate n tail