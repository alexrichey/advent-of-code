import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map as Map

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_03/data.txt" ReadMode
        raw <- hGetContents handle

        let letterVals = Map.fromList $ (zip ['a'..'z'] [1..]) ++ (zip ['A'..'Z'] [27..])

        -- Part 1
        -- let contents = words raw
        --     partitionedContents = map (\x -> splitAt ((length x) `div` 2) x) contents
        --     dupedLetters = map (\x -> head $ intersect (fst x) (snd x)) partitionedContents
        --     vals = map (\c -> Map.lookup c letterVals) dupedLetters

        -- Part 2
        let contents = words raw
            partitionedContents = chunksOf 3 contents
            dupedLetters = map commonChar partitionedContents
            vals = map (\c -> Map.lookup c letterVals) dupedLetters

        print $ fmap sum $ sequence vals

        hClose handle


commonChar :: [String] -> Char
commonChar [a,b,c] = head $ intersect a (intersect b c)
