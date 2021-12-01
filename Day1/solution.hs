import System.IO  
import Control.Monad

solutionOne :: [Int] -> Int
solutionOne (a:b:t) 
    | b > a = 1 + solutionOne (b:t)
    | otherwise = solutionOne (b:t)
solutionOne a = 0

solutionTwo :: [Int] -> Int
solutionTwo (a:b:c:d:t) 
    | d > a = 1 + solutionTwo (b:c:d:t)
    | otherwise = solutionTwo (b:c:d:t)
solutionTwo a = 0

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let input = (fmap read) $ words contents
    print $ solutionOne input
    print $ solutionTwo input