import System.IO 
import Control.Monad

stringToValList :: [Char] -> [Int]
stringToValList s = fmap f s
    where f x = 2 * val - 1 
            where val = fromEnum x - fromEnum '0'

sumLists :: [[Int]] -> [Int] -> [Int]
sumLists (a:t) acc = sumLists t $ zipWith (+) acc a
sumLists a acc = acc

sumFirst :: [[Int]] -> Int -> Int
sumFirst (a:t) acc = sumFirst t $ acc + (head a)
sumFirst a acc = acc

filterLists :: [[Int]] -> Int -> [[Int]]
filterLists (a:t) x = 
    if head a == x then (tail a) : filterLists t x else filterLists t x 
filterLists a x = []

getSecondRate :: [[Int]] -> (Int -> Int) -> [Int] -> Int
getSecondRate (a:[]) _ res = getRate ((reverse res) ++ a) 1 0
getSecondRate (a:t) fun res = getSecondRate (filterLists (a:t) chosen) fun (chosen : res) 
    where chosen = fun (sumFirst (a:t) 0)

solutionOne :: [[Int]] -> Int
solutionOne (a:t) = getRate l 1 0 * getRate l 0 1
    where l =  sumLists t $ replicate (length a) 0

solutionTwo :: [[Int]] -> Int
solutionTwo l = getSecondRate l getMajority [] * getSecondRate l getMinority []

getRate :: [Int] -> Int -> Int -> Int
getRate l a b = rate
    where (_, rate) = fromBinary (fmap (\x -> if x > 0 then a else b) l)

getMajority :: Int -> Int
getMajority x = if x >= 0 then 1 else -1

getMinority :: Int -> Int
getMinority x = if x >= 0 then -1 else 1

fromBinary :: [Int] -> (Int, Int)
fromBinary (a:t) = (prevMul * 2, res + prevMul * a)
    where (prevMul, res) = fromBinary t
fromBinary [] = (1, 0)

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let lists = fmap stringToValList $ words contents
    print $ solutionOne lists
    print $ solutionTwo lists