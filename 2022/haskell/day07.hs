import System.IO
import Data.List
import qualified Data.Map as Map
import Data.String
import Data.List.Split
import qualified Data.Tree as Tree
import Data.Typeable

main = do
   handle <- openFile "../inputs/day_07/input_test.txt" ReadMode
   raw <- hGetContents handle
   let lines = splitOn "\n" raw
       terminalOutput = map parseOutput (filter (/="") lines)
       accumed = snd $ foldl accumTermOutput ([], []) terminalOutput

       -- we'll do something 'clever' here. We have pairs of (cursor, fileSize) like
       -- (["/", "dir1", "dir2"], 1234). Since the size 1234 rolls up to
       -- ["/"], ["/", "dir1"] and ["/", "dir1", "dir2"], we'll use inits create those groups and
       -- add the size to them all
       sizeApportioner (cursor, size) = [(curs, size) | curs <- inits cursor]
       grouped = groupBy (\a b -> fst a == fst b) $ sort $ concatMap sizeApportioner accumed
       -- [
       --   [([],584),([],2557)...],
       --   [(["a"],584),(["a"],2557),(["a"],29116),(["a"],62596)],
       --   [(["a","e"],584)],
       --   [(["d"],4060174),(["d"],5626152),(["d"],7214296),(["d"],8033020)]
       -- ]

       sizesSummed = sort $ map (\x -> (sum $ map snd x, (fst . head) x)) grouped
       -- [(584,["a","e"]),(94853,["a"]),(24933642,["d"]),(48381165,[])]

       rootDirSize = (fst . last) sizesSummed
       totalSpace = 70000000
       neededSpace = 30000000
       spaceToFree = neededSpace - totalSpace - rootDirSize

   print $ take 1 $ filter (\x -> fst x > spaceToFree) sizesSummed


accumTermOutput:: (DirectoryCursor, [(DirectoryCursor, Int)]) -> TerminalOutput -> (DirectoryCursor, [(DirectoryCursor, Int)])
accumTermOutput inPair@(dirCursor, builtUp) terminalOutput = case terminalOutput of
  Printed s    -> (dirCursor, builtUp ++ [(dirCursor, size)])
    where size = (read . head . splitOn " ") s
  CD "/"       -> inPair
  CD ".."      -> (init dirCursor, builtUp)
  CD dir       -> (dirCursor ++ [dir], builtUp)
  LS           -> inPair
  DIR _        -> inPair
  _            -> inPair

parseOutput :: String -> TerminalOutput
parseOutput ('d':'i':'r':' ':xs)  = None
parseOutput ('$':' ':xs)  = constructCmd xs
parseOutput s  = Printed s

constructCmd :: String -> TerminalOutput
constructCmd s | s == "ls"= LS
               | "dir " `isPrefixOf` s = DIR (drop 4 s)
               | "cd "  `isPrefixOf` s = CD (drop 3 s)
               | otherwise = None

type DirectoryCursor = [String]

data TerminalOutput = Printed String | CD String | LS | DIR String | None deriving (Show)
