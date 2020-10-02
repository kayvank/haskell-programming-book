-- | 

module QSort where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ rhs
  where lhs = filter ( < x ) xs
        rhs = filter (> x) xs
    
