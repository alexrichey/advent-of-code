import System.IO
import Data.Char (isSpace)
import Data.Function (on)
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Map as Map
import Text.Regex

main = do
   handle <- openFile "../inputs/day_15/input.txt" ReadMode
   raw <- hGetContents handle
   let lines = splitOn "\n" raw
       pairs = catMaybes $ map parseInputLine lines

       -- Blah
       minX = minimum [(fst . sensor) $ minimumBy (compare `on` (fst . sensor)) pairs,
                       (fst . beacon) $ minimumBy (compare `on` (fst . beacon)) pairs]
       maxX = maximum [(fst . sensor) $ maximumBy (compare `on` (fst . sensor)) pairs,
                       (fst . beacon) $ maximumBy (compare `on` (fst . beacon)) pairs]

       targetY = 2000000
       layerSlices = mergeRanges $ catMaybes $ map (sensorLayerSlice targetY) pairs

       beaconsInSlice = filter (\p -> (snd . beacon) p == targetY) pairs
       beaconRanges = mergeRanges $ map (SingletonRange . snd . beacon) beaconsInSlice

   print $ length $ fromRanges $ difference layerSlices beaconRanges


-- Given a targetDepth (y), determine the slices of x in which the sensor would find beacons
sensorLayerSlice :: Int -> Pair -> Maybe (Range Int)
sensorLayerSlice targetDepth pair =
  if maxDist < abs (targetDepth - sY)
  then Nothing
    else Just ((sX - (maxDist - (abs (sY - targetDepth))))
              +=+
              (sX + (maxDist - (abs (sY - targetDepth)))))
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

-- Make a lil diamond around the given coord
coordsInRange :: Coord -> Int -> [Coord]
coordsInRange (cX, cY) range = concat zs
  where zs = [
                [(x, y) | x <- [cX - (range - (abs (cY - y))) .. cX + (range - (abs (cY - y)))]]
              | y <- [cY-range..cY+range]]

taxiDistance :: Coord -> Coord -> Int
taxiDistance (x1, x2) (y1, y2) = (abs $ x1 - y1) + (abs $ x2 - y2)

type Coord = (Int, Int)
data Pair = Pair {sensor :: Coord, beacon :: Coord} deriving (Show)
