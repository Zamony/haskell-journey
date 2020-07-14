import qualified Data.List
import qualified Data.Char

numUniques :: (Eq a)=>[a]->Int
numUniques = \x-> (length (Data.List.nub x))

wordsCount :: String->[(String, Int)]
wordsCount s = 
    map (\g -> (head g, (length g))) group
    where
        group = Data.List.group $ Data.List.sort $ Data.List.words s

subList :: [Int]->[Int]->Bool
subList [] _ = True
subList _ [] = False
subList (x:xs) (y:ys)
    | x == y = (subList xs ys)
    | otherwise = False

subListSearch :: [Int]->[Int]->Bool
subListSearch ndl hst = foldl (||) False matches
    where matches = [subList ndl y | y<-(Data.List.tails hst)]

caesarEncode :: Int->[Char]->[Char]
caesarEncode n str = [Data.Char.chr $ (+n) $ Data.Char.ord c | c<-str]

caesarDecode :: Int->[Char]->[Char]
caesarDecode n str = [Data.Char.chr $ (subtract n) $ Data.Char.ord c | c<-str]

digitSum :: Integer->Int
digitSum n = sum $ map Data.Char.digitToInt $ show n

valueByKey :: (Eq k)=>[(k, v)]->k->Maybe v
valueByKey [] _ = Nothing
valueByKey haystack ndl
    | null needles = Nothing
    | otherwise = Just $ snd $ head $ needles
    where needles = filter (\(k, _)->k==ndl) haystack