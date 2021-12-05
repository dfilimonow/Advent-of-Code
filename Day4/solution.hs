import System.IO 
import Control.Monad

parseHead :: String -> [Int]
parseHead s = parsePom s [] ""
    where parsePom (h:t) res acc
              | h == ',' = parsePom t ((read $ reverse acc) : res) ""
              | otherwise = parsePom t res (h : acc)
          parsePom [] res _ = reverse res

parseTail :: [String] -> [[[Int]]]
parseTail s = parsePom s [] [] [] 0 0
    where parsePom (h:t) res accRow accColumn row column
              | column == 4 && row == 4 = parsePom t ((((read h) : accRow) : accColumn) : res) [] [] 0 0
              | row == 4 = parsePom t res [] (((read h) : accRow) : accColumn) 0 (column + 1)
              | otherwise = parsePom t res ((read h) : accRow) accColumn (row + 1) column
          parsePom [] res _ _ _ _= reverse res

addColumns :: [[[Int]]] -> [[[Int]]]
addColumns (h:t) = ((h ++ (addColumnsPom h)) : addColumns t)
    where addColumnsPom ([]:t) = []
          addColumnsPom (h:t) = ((getFirstColumn (h:t) []) : addColumnsPom (cut (h:t)))
          getFirstColumn (a:t) acc = getFirstColumn t ((head a) : acc)
          getFirstColumn a acc = acc
          cut (a:t) = ((tail a) : cut t)
          cut [] = []
addColumns [] = []

solutionOne :: [Int] -> [[[Int]]] -> Int
solutionOne l d = solution l [] d 
    where solution (h:t) added d
              | result == -1 = solution t (h:added) d
              | otherwise = result
              where result = getResult (h:added) d
                    getResult added (h:t)
                        | ifWins added h = (head added) * (div (calc added h) 2)
                        | otherwise = getResult added t
                    getResult added [] = -1
                    ifWins added (h:t)
                        | allExist added h = True
                        | otherwise = ifWins added t
                    ifWins added [] = False
                    allExist added (h:t)
                        | elem h added = allExist added t
                        | otherwise = False
                    allExist added [] = True
                    calc added (h:t) = sumElem added h + calc added t
                    calc _ [] = 0   
                    sumElem added (h:t) 
                        | elem h added = sumElem added t
                        | otherwise = h + sumElem added t
                    sumElem added [] = 0

solutionTwo :: [Int] -> [[[Int]]] -> Int
solutionTwo l d = solution l [] d d
    where solution (h:t) added d filtered
              | result == -1 = solution t (h:added) filtered refiltered
              | otherwise = result
              where result = getResult (h:added) filtered refiltered
                    getResult added (h:t) fil
                        | length fil == 0 && ifWins added h = (head added) * (div (calc added h) 2)
                        | otherwise = -1
                    getResult _ _ _ = -1
                    ifWins added (h:t)
                        | allExist added h = True
                        | otherwise = ifWins added t
                    ifWins added [] = False
                    allExist added (h:t)
                        | elem h added = allExist added t
                        | otherwise = False
                    allExist added [] = True
                    calc added (h:t) = sumElem added h + calc added t
                    calc _ [] = 0   
                    sumElem added (h:t) 
                        | elem h added = sumElem added t
                        | otherwise = h + sumElem added t
                    sumElem added [] = 0  
                    filt z added = filter (\x -> (not (ifWins added x))) z
                    refiltered = (filt filtered (h:added))

main = do
    h <- openFile "data.txt" ReadMode
    contents <- hGetContents h
    let input = words contents

    print $ solutionOne (parseHead $ head $ input) (addColumns (parseTail $ tail $ input))
    print $ solutionTwo (parseHead $ head $ input) (addColumns (parseTail $ tail $ input))
    