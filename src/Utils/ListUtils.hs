module Utils.ListUtils
  ( modifyElement,
    transpose,
    enumerate,
  )
where

modifyElement n val list = take n list ++ [val] ++ drop (n+1) list

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

enumerate :: [a] -> [b] -> [(a,b)]
enumerate l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
  where 
    enumerateWithAcc _ [] [] _ = []
    enumerateWithAcc itemL1 [] l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
    enumerateWithAcc itemL1 (h:rest) l1 l2 = (itemL1,h) : enumerateWithAcc itemL1 rest l1 l2