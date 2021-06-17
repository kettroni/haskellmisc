maxNFromList :: (Ord a) => [a] -> a -> [a]
maxNFromList xs n = [ if x > n then n else x | x <- xs]
