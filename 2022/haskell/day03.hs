import System.IO
import Data.List.Split
import Data.List
import qualified Data.Map as Map

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_03/data.txt" ReadMode
        raw <- hGetContents handle

        let letterVals = Map.fromList $ (zip ['a'..'z'] [1..]) ++ (zip ['A'..'Z'] [27..])

        let contents = words raw
            partitionedContents = map (\x -> splitAt ((length x) `div` 2) x) contents
            dupedLetters = map (\x -> head $ intersect (fst x) (snd x)) partitionedContents
            vals = map (\c -> Map.lookup c letterVals) dupedLetters

        print $ fmap sum $ sequence vals

        hClose handle
