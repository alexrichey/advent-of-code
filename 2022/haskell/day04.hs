import System.IO
import Data.List.Split
import Data.List
import qualified Data.Text as Text
import qualified Data.Map as Map

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_04/data.txt" ReadMode
        raw <- hGetContents handle

        let ranges = filter (/= "") (splitOn "\n" raw)
            groups = map parseAssignmentGroup ranges
            withOverlap = filter (\r -> hasOverlap (head r) (last r)) groups

        print $ length withOverlap
        hClose handle


parseAssignmentGroup :: String -> [[Int]]
parseAssignmentGroup s = [head parts, last parts]
  where commaDelimd = Text.unpack $ Text.replace (Text.pack "-") (Text.pack ",") (Text.pack s) -- I hate this
        asNumeric = map read (splitOn "," commaDelimd) :: [Int]
        parts = chunksOf 2 asNumeric

containsRange :: [Int] -> [Int] -> Bool
containsRange [supL,supR] [subL,subR] = (supL <= subL) && (supR >= subR)

isInRange :: [Int] -> Int -> Bool
isInRange [r1,r2] i = (i >= r1) && (i <= r2)

hasOverlap :: [Int] -> [Int] -> Bool
hasOverlap r1@[r1Left,r1Right] r2@[r2Left, r2Right] = (eitherContains r1 r2) || (isInRange r2 r1Left) || (isInRange r2 r1Right)

eitherContains :: [Int] -> [Int] -> Bool
eitherContains r1 r2 = containsRange r1 r2 || containsRange r2 r1
