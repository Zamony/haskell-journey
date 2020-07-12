mulThree :: Int->Int->Int->Int
mulThree x y z = x * y * z

applyTwice :: (a->a)->a->a
applyTwice f x = (f (f x))

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

flip' :: (a->b->c)->(b->a->c)
flip' f a b = f b a

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
    | v = x:(filter' f xs)
    | otherwise = (filter' f xs)
    where v = f x

takeWhile' :: (a->Bool)->[a]->[a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:(takeWhile' f xs)
    | otherwise = []

sum' :: (Num a)=>[a]->a
sum' xs = foldl (+) 0 xs

elem' :: (Eq a)=>a->([a]->Bool)
elem' x = foldl (\m y -> m || (y == x)) False

dropWhile' :: (a->Bool)->[a]->[a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

reverse' :: [a]->[a]
reverse' xs = foldl (\l x-> x:l) [] xs