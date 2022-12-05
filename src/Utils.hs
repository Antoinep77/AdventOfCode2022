module Utils
  ( split,
    modifyElement,
    debug,
  )
where

import Debug.Trace

debug = flip trace


split :: String -> String -> [String]
split sep str = filter (/= "") (splitWithAcc "" sep str)
  where
    splitWithAcc :: String -> String -> String -> [String]
    splitWithAcc acc sep "" = [reverse acc]
    splitWithAcc acc sep str
      | startWith sep str = reverse acc : splitWithAcc "" sep (drop (length sep) str)
      | otherwise =  splitWithAcc (head str : acc) sep (tail str)


startWith :: String -> String -> Bool
startWith "" _ = True
startWith _ "" = False
startWith (h:t) (h2:t2) = h == h2 && startWith t t2

modifyElement n val list = take n list ++ [val] ++ drop (n+1) list

