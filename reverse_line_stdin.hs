main = do
    putStrLn "Enter a line to reverse:"
    line <- getLine
    if null line then
        return ()
    else do
        putStrLn ( unwords $ map reverse $ words line)
        main
