quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x]
quicksort' xs =
    (quicksort' left) ++ (replicate count mid) ++ (quicksort' right)
    where mid = xs !! ((length xs) `div` 2)
          left = [x | x<-xs, x < mid]
          right = [x | x<-xs, x > mid]
          count = sum [1 | x<-xs, x == mid]