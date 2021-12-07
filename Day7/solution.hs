import System.IO  
import Data.List.Split

solutionOne :: [Int] -> Int
solutionOne l = solution [0..(foldr max 0 l)] l maxBound id

solutionTwo :: [Int] -> Int
solutionTwo l = solution [0..(foldr max 0 l)] l maxBound (\x -> div (x * (x + 1)) 2)
    
solution :: [Int] -> [Int] -> Int -> (Int -> Int) -> Int 
solution (h:t) list acc f = solution t list (min acc (calc h list 0)) f
    where calc val (h:t) acc = calc val t (acc + f (abs (h - val)))
          calc _ [] acc = acc
solution [] _ acc _ = acc
    
main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let input = (fmap read) (splitOn "," $ head $ words contents)
    print $ solutionOne $ input
    print $ solutionTwo $ input