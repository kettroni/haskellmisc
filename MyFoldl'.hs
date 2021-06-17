foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ x [] = x
foldl' f x (y:ys) = foldl' f (f x y) ys
