module Utils
  ( split,
    modifyElement,
    debug,
    getOrElse,
    transpose,
    enumerate,
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

getOrElse :: Maybe a -> a -> a
getOrElse Nothing x = x
getOrElse (Just x) _ = x

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

enumerate :: [a] -> [b] -> [(a,b)]
enumerate l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
  where 
    enumerateWithAcc _ [] [] _ = []
    enumerateWithAcc itemL1 [] l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
    enumerateWithAcc itemL1 (h:rest) l1 l2 = (itemL1,h) : enumerateWithAcc itemL1 rest l1 l2