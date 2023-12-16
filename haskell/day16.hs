module Day16 where
import qualified Aoc
import Data.Array
import qualified Data.Array.ST as S
import Control.Monad.ST
import Control.Monad

type Coord = (Int, Int)
type MirrorMap = Array Coord Char
data Direction = North | South | East | West deriving (Show, Eq)

parse :: [String] -> MirrorMap 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)
  
move :: Coord -> Direction -> Coord
move (x,y) North = (x, y-1)
move (x,y) South = (x, y+1)
move (x,y) West = (x-1, y)
move (x,y) East = (x+1, y)

reflect :: Char -> Direction -> [Direction]  
reflect '/' North = [East]
reflect '/' South = [West]
reflect '/' East = [North]
reflect '/' West = [South]
reflect '\\' North = [West]
reflect '\\' South = [East]
reflect '\\' East = [South]
reflect '\\' West = [North]
reflect '|' North = [North]
reflect '|' South = [South]
reflect '|' East = [North, South]
reflect '|' West = [North, South]
reflect '-' North = [East, West]
reflect '-' South = [East, West]
reflect '-' East = [East]
reflect '_' West = [West]
reflect _ d = [d]
  
moveBeam m bm p dir = do
  when (inRange (bounds m) p) $ do
    dirs <- S.readArray bm p
    unless (dir `elem` dirs) $ do
      S.writeArray bm p (dir : dirs)
      mapM_ (\d -> moveBeam m bm (move p d) d) (reflect (m ! p) dir)

runBeam :: MirrorMap -> Coord -> Direction -> Int
runBeam m p dir = runST $ do
  bm <- S.newArray (bounds m) [] :: ST s (S.STArray s Coord [Direction])
  moveBeam m bm p dir
  elems <- S.getElems bm
  return (length $ filter (not . null) elems)
  
startingPoints :: MirrorMap -> [(Coord, Direction)]
startingPoints m =
  let (w, h) = snd $ bounds m in
  [((x, 1), South) | x <- [1..w]] ++
  [((1, y), East) | y <- [1..h]] ++
  [((x, h), North) | x <- [1..w]] ++
  [((y, w), West) | y <- [1..h]]
  
part1 m = runBeam m (1,1) East
part2 m = maximum $ map (uncurry (runBeam m)) (startingPoints m)
run = Aoc.run 16 parse part1 part2
