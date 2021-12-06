import System.IO  
import Data.List.Split
import Data.List

simulateDay :: [(Int, Int)] -> [(Int, Int)]
simulateDay list = (newFish, 8) :  newList
    where (newFish, newList) = foldr (\(m, x) (n, l) -> if x == 0 then (n + m, (m, 6):l) else (n, (m, x - 1):l)) (0, []) list

solutionOne :: [(Int, Int)] -> Int
solutionOne s = mySum $ foldr ($) s $ replicate 80 simulateDay 

solutionTwo :: [(Int, Int)] -> Int
solutionTwo s = mySum $ foldr ($) s $ replicate 256 simulateDay 

count :: [[Int]] -> [(Int, Int)]
count l = fmap (\x -> (length x, head x)) l

mySum :: [(Int, Int)] -> Int
mySum l = foldr (\(n,_) prev -> prev + n) 0 l

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let input = count (groupBy (==) (sort $ ((fmap read) (splitOn "," $ head $ words contents))))
    print $ solutionOne $ input
    print $ solutionTwo $ input