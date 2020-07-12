maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max on empty list"
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)

replicate' :: a->Int->[a]
replicate' a 1 = [a]
replicate' a n
    | n <= 0 = error "replicate n <= 0"
    | otherwise = a:(replicate' a (n-1))

take' :: Int->[a]->[a]
take' 0 _ = []
take' 1 arr
    | null arr = []
    | otherwise = [head arr]
take' n arr
    | n < 0 = error "take count n <= 0"
    | null arr = []
    | otherwise = [head arr] ++ (take' (n-1) (tail arr))

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [a] = [a]
reverse' (x:xs) = reverse'(xs) ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat'(a)

zip' :: [a]->[b]->[(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

elem' :: (Eq a)=>a->[a]->Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys