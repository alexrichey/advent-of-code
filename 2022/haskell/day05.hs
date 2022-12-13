import System.IO
import Data.List.Split
import Data.List
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Map as Map

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_05/data.txt" ReadMode
        raw <- hGetContents handle
        let lines = splitOn "\n" raw
            parsed@(rawCrates, rawInstrs) = parseInput lines
            stackedCrates = stackCrates $ parseCrates rawCrates
            instructions = map parseInstruction rawInstrs
            movedCrates = foldl move stackedCrates instructions

        print $ map head movedCrates

        hClose handle

move :: [[Char]] -> Instruction -> [[Char]]
move crates i = map moveCrates (zip [1..] crates)
  where toMove = reverse $ take (moveCount i) (crates !! (fromCol i - 1))
        moveCrates (idx, stack)
          | idx == fromCol i = drop (length toMove) stack
          | idx == toCol i   = toMove ++ stack
          | otherwise        = stack

data Instruction = Instruction {moveCount :: Int
                               , fromCol :: Int
                               , toCol :: Int} deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = Instruction (read cnt) (read frm) (read to)
  where [_,cnt,_,frm,_,to] = splitOn " " s

-- bad...
unboxMaybe :: Maybe Char -> Char
unboxMaybe (Just x) = x

stackCrates :: [[Maybe Char]] -> [[Char]]
stackCrates cs = unboxed
  where stacked = transpose cs
        filtered = map (filter (/= Nothing)) stacked
        unboxed = map (map unboxMaybe) filtered -- not great...

parseCrateRow :: String -> Maybe Char
parseCrateRow ['[',a,']'] = Just a
parseCrateRow "" = Nothing

parseCrateLine :: String -> [Maybe Char]
parseCrateLine s = parsed
  where chunked = chunksOf 4 s
        cleaned = map trim chunked
        parsed = map parseCrateRow cleaned

parseCrates :: [String] -> [[Maybe Char]]
parseCrates xs = map parseCrateLine crateLines
  where crateLines = take (length xs -1) xs

parseInput :: [String] -> ([String], [String])
parseInput xs = (crates, instrs)
  where crates = takeWhile (/= "") xs
        instrs = filter (/= "") $ drop (length crates + 1) xs

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
