quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallSorted = quicksort [ y | y <- xs, y < x]
      bigSorted = quicksort [ y | y <- xs, y >= x]
  in smallSorted ++ [x] ++ bigSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallSorted = quicksort' (filter (<x) xs)
      bigSorted = quicksort' (filter (>=x) xs)
  in smallSorted ++ [x] ++ bigSorted
