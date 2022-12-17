import System.IO
import Data.Function (on)
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.Console.ANSI
import Control.Concurrent
import Control.Monad

main = do
        handle <- openFile "../inputs/day_14/data.txt" ReadMode
        raw <- hGetContents handle

        let rangesRaw = splitOn "\n" raw
            ranges = map parseRangeInput rangesRaw
            connectedRocks = concatMap (foldl rangeConj []) ranges
            rockyBoard = Board { tiles=Map.fromList (map (, Rock) connectedRocks)
                               , sandEntry=(500,0)
                               , bounds=boardBounds connectedRocks
                               , tickedFrames=0
                               , sandSentToVoidCount=0
                               , sandCounter=0
                               , movingSand=Nothing
                               , explainLastFrame=""}

        -- animate this sucker
        -- forM (iterate animateFrame rockyBoard) (\f -> do
        --                                            clearScreen
        --                                            mapM print (drawBoard f)
        --                                            threadDelay 100000)

        let final = head $ dropWhile (\b -> not $ spoutIsBlocked b) (iterate animateFrame rockyBoard)
        mapM print (drawBoard final)
        print $ sandCounter final
        print $ explainLastFrame final

animateFrame :: Board -> Board
animateFrame b
  | spoutIsBlocked b = b { movingSand=Nothing
                         , tickedFrames=tickedFrames b + 1
                         , explainLastFrame="Spout is blocked"}
  | isNothing (movingSand b) = b { movingSand=Just (sandEntry b)
                                 , sandCounter=sandCounter b + 1
                                 , tickedFrames=tickedFrames b + 1
                                 , explainLastFrame="Popping a new sand block onto the board"}
  | otherwise = newBoard
      where coord@(rght,dwn) = fromJust (movingSand b)
            down =      (rght,   dwn+1)
            downLeft =  (rght-1, dwn+1)
            downRight = (rght+1, dwn+1)
            nextMoves = [down, downLeft, downRight]
            maybeNextMoveIndex = elemIndex Nothing (map (getElemAtCoord b) nextMoves)
            newBoard = case maybeNextMoveIndex of
              -- Nowhere to move. Settle
              Nothing -> b { movingSand=Nothing
                           , tiles=Map.insert coord Sand (tiles b)
                           , tickedFrames=tickedFrames b + 1
                           , explainLastFrame="Settled Sand at "++ (show coord) ++ " found: " ++ (show nextMoves)}
              -- Moving some sand
              _       -> b { movingSand=Just nextMove
                           , tickedFrames=tickedFrames b + 1
                           , explainLastFrame="Moved Sand to " ++ show nextMove}
                         where nextMove = (nextMoves !! fromJust maybeNextMoveIndex)

-- "498,4 -> 498,6 -> 496,6" -> [(498,4), (498,6), (496,6)]
parseRangeInput :: String -> [Coord]
parseRangeInput "" = []
parseRangeInput x = map parseCoord $ splitOn " -> " x
  where
    parseCoord :: String -> Coord
    parseCoord c = (read $ head split, read $ last split)
        where split = splitOn "," c


-- Connects list of input coords
rangeConj :: [Coord] ->  Coord -> [Coord]
rangeConj [] rangeEnd = [rangeEnd]
rangeConj coords rangeEnd = coords ++ drop 1 (connectCoords (last coords) rangeEnd)
  where
    -- (1,2) (1,4) -> [(1,2), (1,3), (1,4)]
    connectCoords :: Coord -> Coord -> [Coord]
    connectCoords from to = [(x,y) | x <- fst from ..<> fst to,
                                     y <- snd from ..<> snd to]

--  I know... Super inneficient.
boardBounds :: [Coord] -> (Coord, Coord)
boardBounds cs = ((leftMost, 0),
                  (rightMost,bottomMost))
  where leftMost =   fst $ minimumBy (compare `on` fst) cs
        rightMost =  fst $ maximumBy (compare `on` fst) cs
        bottomMost = snd $ maximumBy (compare `on` snd) cs

data Material = Rock | Sand | SandEntry | MovingSand deriving (Show, Eq)
type Tiles = Map.Map Coord Material
type Coord = (Int, Int)

data Board = Board { tiles :: Tiles
                   , sandEntry :: Coord
                   , bounds :: (Coord, Coord) -- topLeft, bottomRight
                   , movingSand :: Maybe Coord
                   , sandCounter :: Int
                   , tickedFrames :: Int
                   , sandSentToVoidCount :: Int
                   , explainLastFrame :: String} deriving (Show)

getElemAtCoord :: Board -> Coord -> Maybe Material
getElemAtCoord b coord@(x,y) | y >= bottom + 2 = Just Rock
                             | otherwise   = Map.lookup coord (tiles b)
  where bottom = snd (snd (bounds b))

spoutIsBlocked :: Board -> Bool
spoutIsBlocked b = getElemAtCoord b (sandEntry b) == Just Sand


drawBoard :: Board -> [String]
drawBoard b = displayed ++ ["frames: " ++ show (tickedFrames b), "sent to void: " ++ show (sandSentToVoidCount b)]
  where
    tilesWithSandEntry = Map.insert (sandEntry b) SandEntry (tiles b)
    tilesWithMovingSand = if isNothing (movingSand b) then tilesWithSandEntry
                                                        else Map.insert (fromJust (movingSand b)) MovingSand tilesWithSandEntry


    legend :: Maybe Material -> String
    legend x = case x of Just Rock -> "#"
                         Just Sand -> "o"
                         Just SandEntry -> "~"
                         Just MovingSand -> "+"
                         Nothing -> " "

    ((leftMost, _), (rightMost, bottomMost)) = bounds b
    xrange = [leftMost..rightMost]
    yrange = [0..bottomMost + 2]
    charGrid = [map (\x -> legend $ Map.lookup (x,y) tilesWithMovingSand) xrange | y <- [0..bottomMost]]
    displayed = map unwords charGrid

-- :grimace:
-- Bidirectional range. Ie. you can also do [10..1], which you can't with plain ..
(..<>) :: Int -> Int -> [Int]
(..<>) a b = if a <= b then [a..b]
                        else reverse [b..a]
