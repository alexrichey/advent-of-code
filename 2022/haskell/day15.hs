import System.IO
import Data.Function (on)
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Range
import qualified Control.Spoon as Spoon
import Text.Regex

main = do
   handle <- openFile "../inputs/day_15/input_test.txt" ReadMode
   raw <- hGetContents handle
   let lines = splitOn "\n" raw
       pairs = mapMaybe parseInputLine lines

       -- Blah
       minX = min [(fst . sensor) $ minimumBy (compare `on` (fst . sensor)) pairs,
                   (fst . beacon) $ minimumBy (compare `on` (fst . beacon)) pairs]
       maxX = max [(fst . sensor) $ maximumBy (compare `on` (fst . sensor)) pairs,
                   (fst . beacon) $ maximumBy (compare `on` (fst . beacon)) pairs]
       inBounds = [0 +=+ 20]

       unMaybe :: Maybe [Int] -> [Int]
       unMaybe x = case x of Nothing -> []
                             _       -> fromJust x

       -- BLAH
       sliceGenerator :: Int -> [Int]
       sliceGenerator y = take 5 -- dumb hack to see whether the range has elements
                          $ unMaybe
                          $ (Spoon.spoon . fromRanges) -- fromRanges is buggy :(
                          $ difference inBounds -- ensure everything is in bounds
                          $ mergeRanges
                          $ mapMaybe (sensorLayerSlice y) pairs

   print $ head (dropWhile (null . fst)
                  (zip [sliceGenerator y | y <- [0..]]
                        [0..]))



-- Given a targetDepth (y), determine the slices of x in which the sensor would find beacons
sensorLayerSlice :: Int -> Pair -> Maybe (Range Int)
sensorLayerSlice targetDepth pair =
  if maxDist < abs (targetDepth - sY)
  then Nothing
    else Just ((sX - (maxDist - abs (sY - targetDepth)))
              +=+
              (sX + (maxDist - abs (sY - targetDepth))))
  where
    maxDist = taxiDistance (sensor pair) (beacon pair)
    (sX, sY) = sensor pair


-- e.g. "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
parseInputLine :: String -> Maybe Pair
parseInputLine "" = Nothing
parseInputLine s = Just Pair {sensor=(sx,sy), beacon=(bx,by)}
  where r = mkRegex "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)"
        matches :: [Int] = map read $ fromJust (matchRegex r s)
        ([sx, sy], [bx, by]) = splitAt 2 matches

taxiDistance :: Coord -> Coord -> Int
taxiDistance (x1, x2) (y1, y2) = abs (x1 - y1) + abs (x2 - y2)

type Coord = (Int, Int)
data Pair = Pair {sensor :: Coord, beacon :: Coord} deriving (Show)
