import System.IO
import Data.List.Split
import Data.List

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_01/data.txt" ReadMode
        contents <- hGetContents handle

        let calorieWords = splitOn "\n" contents

        print(maximum
              (map sum
               (partitionCalsByElf calorieWords)))

        hClose handle

-- Takes an input list of calories, and partitions calories by elf,
-- delimited by empty strings
-- e.g. ["1", "2", "", "3"] -> [[1, 2], [3]]
partitionCalsByElf :: [String] -> [[Int]]
partitionCalsByElf x = _partitionCalsByElf x [] []

_partitionCalsByElf :: [String] -> [Int] -> [[Int]]  -> [[Int]]
--                     remaining -> next -> return
_partitionCalsByElf [] [] ret = ret
_partitionCalsByElf [] nxt ret = ret ++ [nxt]
_partitionCalsByElf ("": "": xs) nxt ret = _partitionCalsByElf ("": xs) nxt ret
_partitionCalsByElf ("": xs) [] ret = _partitionCalsByElf xs [] ret
_partitionCalsByElf ("": xs) nxt ret = _partitionCalsByElf xs [] (ret ++ [nxt])
_partitionCalsByElf (x: xs) nxt ret = _partitionCalsByElf xs (nxt ++ [read x]) ret




{-
--- Post exercise reflection

--  Happy Things
- Pattern matching + recursion was really sweet. That convoluted recursive fn
  was correct by the second try. As they say, if it compiles...
- If I move hClose up a few lines, everything breaks. Which means... everything is evaluated lazily?
  I think...?
- HLint suggested some really good simplifications, esp for my lambdas

--  Frustrations:
- Haskell documentation seems to delight in answering basic questions with a phd thesis.
- editor setup (still not complete)
- I'm not convinced that my recursive solution to group the calories is a good one.
  - I probably should have used groupdBy with a regex, but I didn't want to pull in any more libs than needed
  - speaking of which, how on earth do dependencies work in this ecosystem?

-- TODO
- Have actual valid function docs?

-}
