secondHighest' :: (Ord a) => [a] -> a -> a -> a
secondHighest' [] h s = s
secondHighest' (x:xs) h s
  | x >= h    = secondHighest' xs x h
  | x >= s    = secondHighest' xs h x
  | otherwise = secondHighest' xs h s

secondHighest :: (Ord a) => [a] -> a
secondHighest [] = error "List needs at least 2 elements"
secondHighest [x] = error "List needs at least 2 elements"
secondHighest (x1:x2:xs)
  | x1 > x2   = secondHighest' xs x1 x2
  | otherwise = secondHighest' xs x2 x1
