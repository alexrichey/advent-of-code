import System.IO
import Data.List
import qualified Data.Map as Map
import Data.String
import Data.List.Split
import qualified Data.Tree as Tree
import Data.Typeable

main = do
   handle <- openFile "../inputs/day_08/input.txt" ReadMode
   raw <- hGetContents handle
   let treeHeights = words raw
    -- "30373"    (0,1) -> (1,0)
    -- "25512"
    -- "65332"
    -- "33549"
    -- "35390"

       treeHeightsTranspose = transpose treeHeights
    -- "32633"
    -- "05535"
    -- "35353"
    -- "71349"
    -- "32290"

   let visible = [isVisibleInGrid treeHeights (x,y) | x <- [0..(length (head treeHeights)) -1],
                                                      y <- [0..(length treeHeights) -1]]
   print $ length $ filter (== True) visible

isVisibleInSlice :: [Char] -> Int -> (Bool, Bool)
isVisibleInSlice slice pos = (null lft || el  > (maximum lft),
                              null rt  || el  > (maximum rt ))
  where lft = take pos slice
        (el:rt) = drop pos slice

isVisibleInGrid :: [[Char]] -> (Int, Int) -> Bool
isVisibleInGrid tree coords@(x,y) = visL || visR || visU || visD
  where transposeTree = transpose tree
        (visL, visR) = isVisibleInSlice (tree !! y) x
        (visU, visD) = isVisibleInSlice (transposeTree !! x) y
