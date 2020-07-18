import System.Environment

data ExprElem = Plus | Minus | Div | Prod | Number Float deriving(Show)

compute :: [ExprElem] -> Float
compute expr = computeExpr expr []

computeExpr :: [ExprElem] -> [Float] -> Float
computeExpr [] [n] = n
computeExpr ((Number n):elems) stack = computeExpr elems (n:stack)
computeExpr (Plus:elems) (p1:p2:stack) = computeExpr elems ((p1+p2):stack)
computeExpr (Minus:elems) (p1:p2:stack) = computeExpr elems ((p2-p1):stack)
computeExpr (Div:elems) (p1:p2:stack) = computeExpr elems ((p2/p1):stack)
computeExpr (Prod:elems) (p1:p2:stack) = computeExpr elems ((p1*p2):stack)
computeExpr _ _ = error "Invalid expression"

strToExprElem :: String -> ExprElem
strToExprElem "+" = Plus
strToExprElem "-" = Minus
strToExprElem "*" = Prod
strToExprElem "/" = Div
strToExprElem str = Number $ read str

main = do
    args <- getArgs
    if null args then
        putStrLn "You should provide a RPN expression"
    else
        putStrLn $ show $ compute $ map strToExprElem $ words $ head args
