{-# LANGUAGE TupleSections #-}
module Utils.ListUtils
  ( modifyElement,
    transpose,
    enumerate,
    allLinesIteratorWithReverse,
    allLinesIterator
  )
where

modifyElement n val list = take n list ++ [val] ++ drop (n+1) list

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

allLinesIteratorWithReverse :: [[a]] -> [[(a,Int,Int)]]
allLinesIteratorWithReverse grid = lines ++ map reverse lines
  where lines = allLinesIterator grid

allLinesIterator :: [[a]] -> [[(a,Int,Int)]]
allLinesIterator grid = normalLineIterator grid ++ verticalLines
  where
    verticalLines = map (map (\(x,i,j) -> (x,j,i))) $ normalLineIterator $ transpose grid

normalLineIterator :: [[a]] -> [[(a,Int,Int)]]
normalLineIterator grid = map (map (\ (i,(j,x)) -> (x,i,j))) $ zipWith (\x l -> map (x,) l) [0..n] $ map (zip [0..m]) grid
  where n = length grid - 1
        m = length (head grid) - 1

enumerate :: [a] -> [b] -> [(a,b)]
enumerate l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
  where
    enumerateWithAcc _ [] [] _ = []
    enumerateWithAcc itemL1 [] l1 l2 = enumerateWithAcc (head l1) l2 (tail l1) l2
    enumerateWithAcc itemL1 (h:rest) l1 l2 = (itemL1,h) : enumerateWithAcc itemL1 rest l1 l2

