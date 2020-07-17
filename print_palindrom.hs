import Control.Monad

main = interact respondPalindrom

respondPalindrom :: String -> String
respondPalindrom = unlines . map (\s -> if (isPalindrom s) then "palind" else "not palind") . lines

isPalindrom :: String->Bool
isPalindrom l = (reverse l) == l