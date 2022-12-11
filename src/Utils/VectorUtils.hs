module Utils.VectorUtils
  ( manathan,
  )
where


manathan :: [Int] -> [Int] -> Int
manathan vec1 vec2 = sum $ map (\(x,y) -> abs(x - y)) $ zip vec1 vec2