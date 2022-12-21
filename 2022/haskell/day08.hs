import System.IO
import Data.List
import Data.List.Split
import Data.String

main = do
   handle <- openFile "../inputs/day_08/input.txt" ReadMode
   raw <- hGetContents handle
   let treeHeights = words raw
    -- "30373"
    -- "25512"
    -- "65332"
    -- "33549"
    -- "35390"
   print $ maximum [scenicScore treeHeights (x,y) | x <- [0..(length (head treeHeights)) -1],
                                                    y <- [0..(length treeHeights)        -1]]

-- p2
endsAreVisible :: [Char] -> Bool
endsAreVisible [] = True
endsAreVisible (x:[]) = True
endsAreVisible (x:y:[]) = True
endsAreVisible (x:xs) = x > maximum (init xs)

scenicScoreLtoR :: [Char] -> Int
scenicScoreLtoR slice = length $ takeWhile endsAreVisible $ drop 2 $ inits slice

scenicScoreInSlice :: [Char] -> Int -> [Int]
scenicScoreInSlice slice pos = [scenicScoreLtoR $ [el] ++ (reverse lft),
                                scenicScoreLtoR $ [el] ++ rt]
  where lft = take pos slice
        (el:rt) = drop pos slice

scenicScore :: [[Char]] -> (Int, Int) -> Int
scenicScore grid coords@(x,y) = foldl (*) 1 $ leftRight ++ upDown
  where leftRight = scenicScoreInSlice (grid !! y) x
        upDown = scenicScoreInSlice (transpose grid !! x) y

-- p1
isVisibleInSlice :: [Char] -> Int -> (Bool, Bool)
isVisibleInSlice slice pos = (null lft || el  > (maximum lft),
                              null rt  || el  > (maximum rt ))
  where lft = take pos slice
        (el:rt) = drop pos slice

isVisibleInGrid :: [[Char]] -> (Int, Int) -> Bool
isVisibleInGrid grid coords@(x,y) = visL || visR || visU || visD
  where transposeGrid = transpose grid
        (visL, visR) = isVisibleInSlice (grid !! y) x
        (visU, visD) = isVisibleInSlice (transposeGrid !! x) y
