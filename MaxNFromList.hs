maxNFromList :: (Ord a) => [a] -> a -> [a]
maxNFromList xs n = [ if x > n then n else x | x <- xs]

maxNFromListRecursive :: (Ord a) => [a] -> a -> [a]
maxNFromListRecursive [] _ = []
maxNFromListRecursive (x:xs) n
  | x > n     = n : rest
  | otherwise = x : rest
  where rest = maxNFromListRecursive xs n


