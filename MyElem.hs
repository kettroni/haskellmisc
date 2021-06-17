elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs

elemfold :: (Eq a) => a -> [a] -> Bool
elemfold x xs = foldl (\acc b -> if x == b then True else acc) False xs
