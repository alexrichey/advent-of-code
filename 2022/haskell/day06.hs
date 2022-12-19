import System.IO
import Data.List

main = do
        handle <- openFile "../inputs/day_06/data.txt" ReadMode
        raw <- hGetContents handle
        print $ findPacketStart raw 0

findPacketStart :: String -> Int -> Int
findPacketStart packet@(x:xs) startIndex =
  if ((14==) . length . nub . take 14) packet
        then startIndex + 14
        else findPacketStart xs startIndex+1
