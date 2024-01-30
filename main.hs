import System.IO
import Data.Scientific (toRealFloat)

data Mode = NormalMode | SquareRootMode deriving (Show, Eq)
data Operation = Add | Subtract | Multiply | Divide | SquareRoot deriving Show
type Operand = Double

performOperation :: Operation -> Operand -> Operand -> Operand
performOperation Add x y = x + y
performOperation Subtract x y = x - y
performOperation Multiply x y = x * y
performOperation Divide x y = x / y
performOperation SquareRoot x _ = sqrt x

parseOperation :: String -> Operation
parseOperation "+" = Add
parseOperation "-" = Subtract
parseOperation "*" = Multiply
parseOperation "/" = Divide
parseOperation "√" = SquareRoot

calculate :: Operation -> Operand -> Operand -> Operand
calculate op x y = performOperation op x y

getUserInput :: Mode -> IO ()
getUserInput mode = do
    putStrLn $ if mode == SquareRootMode then "Enter a number to calculate its square root:" else "Enter a calculation (e.g., 5 + 3 or 'sqrt' to enter square root mode):"
    line <- getLine
    let tokens = words line
    if mode == SquareRootMode
        then do
            let x = read $ head tokens
            let result = calculate SquareRoot x 0
            putStrLn ("Result: " ++ show result)
            getUserInput mode
        else if line == "sqrt"
            then getUserInput SquareRootMode
            else if length tokens == 3
                then do
                    let [x, op, y] = tokens
                    let result = calculate (parseOperation op) (read x) (read y)
                    putStrLn ("Result: " ++ show result)
                    getUserInput NormalMode
                else putStrLn "Invalid input." >> getUserInput mode

main = do
    hSetBuffering stdin NoBuffering
    getUserInput NormalMode

