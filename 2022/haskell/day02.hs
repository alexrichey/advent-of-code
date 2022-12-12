import System.IO
import Data.List.Split
import Data.List

main = do
        let splitInput = []
        handle <- openFile "../inputs/day_02/data.txt" ReadMode
        contents <- hGetContents handle

        let plays = [x | x <- splitOn "\n" contents, x /= ""]
            pairs = map fromStringPair plays

        print $ sum $ map score pairs

        hClose handle

score :: (RPS, RPS) -> Int
score (theirs, mine) = myHandScore + winScore
  where myHandScore = case mine of Rock -> 1
                                   Paper -> 2
                                   Scissors -> 3
                                   None -> 0
        winScore = case compare mine theirs of LT -> 0
                                               EQ -> 3
                                               GT -> 6

fromStringPair :: String -> (RPS, RPS)
fromStringPair "" = (None, None)
fromStringPair s = (head lst, last lst)
  where
    lst = map fromString (words s)

fromString :: String -> RPS
fromString s = case s of "A"-> Rock
                         "X"-> Rock
                         "B"-> Paper
                         "Y"-> Paper
                         "C"-> Scissors
                         "Z"-> Scissors
                         _  -> None

data RPS = Rock | Paper | Scissors | None deriving (Eq, Show)

instance Ord RPS where
  compare Rock x = case x of
    Paper -> LT
    Scissors -> GT
    _ -> EQ
  compare Paper x = case x of
    Scissors -> LT
    Rock -> GT
    _ -> EQ
  compare Scissors x = case x of
    Rock -> LT
    Paper -> GT
    _ -> EQ
  compare None x = EQ
