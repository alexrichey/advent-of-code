import System.IO
import Data.List

main = do
        handle <- openFile "../inputs/day_06/data.txt" ReadMode
        raw <- hGetContents handle
        print $ findPacketStart raw 0

findPacketStart :: String -> Int -> Int
findPacketStart packet@(x:xs) startIndex =
  if ((4==) . length . nub . take 4) packet
        then startIndex + 4
        else findPacketStart xs startIndex+1
