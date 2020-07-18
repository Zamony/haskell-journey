import System.Environment
import System.IO
import System.Directory

dispatch :: [String]->IO ()
dispatch ["show", filename] = display filename
dispatch ["add", filename, task] = add filename task
dispatch ["remove", filename, task_id] = remove filename task_id
dispatch _ = putStrLn "show filename\nadd filename task\nremove filename task_number"

display :: String -> IO ()
display filename = do
    contents <- readFile filename
    putStrLn (addLineNumbersToText contents)

addLineNumbersToText :: String -> String
addLineNumbersToText = unlines . zipWith (\i s-> (show i) ++ ". " ++ s) [1..] . lines

add :: String->String->IO ()
add filename task = appendFile filename ("\n" ++ task)

remove :: String->String->IO ()
remove filename task_id = do
    contents <- readFile filename
    let new_contents = nthLineFromTextRemoved contents (read task_id)
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle new_contents
    hClose tempHandle
    removeFile filename
    renameFile tempName filename

nthLineFromTextRemoved :: String->Int->String
nthLineFromTextRemoved text n = unlines . map (\(_,s) -> s) . filter (\(i,s) -> i /= n). zipWith (\i s -> (i, s) ) [1..] $ lines text

main = do
    args <- getArgs
    dispatch args