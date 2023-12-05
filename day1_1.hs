module Main where
import Data.Char ( digitToInt, isDigit )

main :: IO ()
main = do solvePuzzle []

solvePuzzle :: [Int] -> IO ()
solvePuzzle ls = getLine >>= \input -> if null input then print (sum ls) else solvePuzzle (10 * findFirstNumber input + findFirstNumber (reverse input) : ls)

findFirstNumber :: String -> Int 
findFirstNumber [] = 0
findFirstNumber (c:cs) = if isDigit c then digitToInt c else findFirstNumber cs
