import System.IO 
import Control.Monad

solutionOne :: ([String], [Integer]) -> Integer
solutionOne (commands, moves) = abs $ horizontal * depth
    where (horizontal, depth) = calculate commands moves
          calculate [] [] = (0, 0)
          calculate (a:c) (b:m)
              | a == "forward" = (x' + b, y')
              | a == "down" = (x', y' - b)
              | a == "up" = (x', y' + b)
              where (x', y') = calculate c m

solutionTwo :: ([String], [Integer]) -> Integer
solutionTwo (commands, moves) = abs $ horizontal * depth
    where (horizontal, depth) = calculate commands moves 0
          calculate [] [] _ = (0, 0)
          calculate (a:c) (b:m) aim
              | a == "forward" = (x' + b, y' + b * aim)
              | a == "down" = calculate c m $ aim - b
              | a == "up" = calculate c m $ aim + b
              where (x', y') = calculate c m aim 

divide :: [String] -> ([String], [Integer])
divide [] = ([], [])
divide (a:b:t) = (a : ta, (read b) : tb)
    where (ta, tb) = divide t

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    print $ solutionOne $ divide $ words contents
    print $ solutionTwo $ divide $ words contents