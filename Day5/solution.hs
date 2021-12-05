import System.IO 
import Control.Monad
import Data.List

parseSingle :: String -> (Int, Int)
parseSingle s = (read (reverse x), read y)
    where (x, y) = parsePom s ""
          parsePom (a:t) acc
            | a == ',' = (acc, t)
            | otherwise = parsePom (t) (a:acc)

parse :: [String] -> Bool -> ([Int], [Int])
parse (a:_:b:t) withDiags
        | ay == by && bx >= ax = ([ax..bx] ++ nextx, replicate (bx - ax + 1) by ++ nexty)
        | ay == by && bx < ax  = ([bx..ax] ++ nextx, replicate (ax - bx + 1) by ++ nexty)
        | ax == bx && by >= ay = (replicate (by - ay + 1) bx ++ nextx, [ay..by] ++ nexty)
        | ax == bx && by < ay  = (replicate (ay - by + 1) bx ++ nextx, [by..ay] ++ nexty)
        | withDiags && ax < bx && ay < by   = ([ax..bx] ++ nextx, [ay..by] ++ nexty)
        | withDiags && ax < bx && ay >= by   = ([ax..bx] ++ nextx, (reverse [by..ay]) ++ nexty)
        | withDiags && ax >= bx && ay < by   = ((reverse [bx..ax]) ++ nextx, [ay..by] ++ nexty)
        | withDiags && ax >= bx && ay >= by   = ([bx..ax] ++ nextx, [by..ay] ++ nexty)
        | otherwise = (nextx, nexty)
    where (nextx, nexty) = parse t withDiags
          (ax, ay) = parseSingle a
          (bx, by) = parseSingle b
parse [] _ = ([], [])

change :: ([Int], [Int]) -> [(Int, Int)]
change ((h:t), (h':t')) = (h, h') : change (t, t')
change ([], []) = []

solutionOne :: [String] -> Int
solutionOne s = count (groupBy (==) (sort $ change $ parse s False))

solutionTwo :: [String] -> Int
solutionTwo s = count (groupBy (==) (sort $ change $ parse s True))

count :: [[a]] -> Int
count (h:t) = foldr (\x prev -> if (length x) == 1 then prev else prev + 1) 0 (h:t)

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let input = words contents
    print $ solutionOne $ input
    print $ solutionTwo $ input