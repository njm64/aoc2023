module Day18 where
import qualified Aoc
import Data.List
import Data.Array
import qualified Data.Array.ST as S
import qualified Data.Map as Map
import Control.Monad.ST
import Numeric

type Command = (Char, Int, String)
type Pos = (Int, Int)
type Axis = Array Int Int

hexToInt :: String -> Int
hexToInt = fst . head . readHex 

parseCommand :: String -> Command
parseCommand s =
  let [dir, n, colour] = words s in
  (head s, read n, init . drop 2 $ colour) 

fixCommand :: Command -> Command
fixCommand (_, _, colour) =
  (dir, n, colour)
  where n = hexToInt . init $ colour
        dir = case last colour of
                '0' -> 'R'
                '1' -> 'D'
                '2' -> 'L'
                '3' -> 'U'
  
move :: Pos -> Char -> Int -> Pos
move (x,y) 'U' n = (x,y-n)
move (x,y) 'D' n = (x,y+n)
move (x,y) 'L' n = (x-n,y)
move (x,y) 'R' n = (x+n,y)
  
-- Convert a list of commands into a list of points
buildPath :: [Command] -> [Pos]
buildPath = foldl' dig [(1,1)]
  where dig (p:ps) (dir, n, c) = move p dir n : p : ps
  
-- This is the key to part 2. We find all the unique points on
-- both the x and y axes, sort them, and map them to small
-- integers starting at 1. Return the path mapped to the new
-- compressed coordinate system, and the x and y axes as arrays.
compressPath :: [Pos] -> ([Pos], Axis, Axis)
compressPath ps = (path, xaxis, yaxis)
  where buildAxis = sort . nub . concatMap (\a -> [a, a+1])
        xs = buildAxis $ map fst ps
        ys = buildAxis $ map snd ps
        xmap = Map.fromList (zip xs [1..])
        ymap = Map.fromList (zip ys [1..])
        path = [(xmap Map.! x, ymap Map.! y) | (x,y) <- ps]
        xaxis = array (1, length xs) (zip [1..] xs)
        yaxis = array (1, length ys) (zip [1..] ys)

-- Connect points in a list with horizontal and vertical lines  
joinPoints :: [Pos] -> [Pos]
joinPoints ps = concatMap (uncurry line) (zip ps (tail ps))
  where line (x1, y1) (x2, y2)
          | x1 == x2 = [(x1, y) | y <- xrange y1 y2]
          | y1 == y2 = [(x, y1) | x <- xrange x1 x2]
        xrange a b
          | a < b = [a..b-1]
          | a > b = [a,a-1..b+1]
          | a == b = [a]
    
pathBounds :: [Pos] -> (Pos, Pos)
pathBounds ps = 
  ((minimum $ map fst ps, minimum $ map snd ps),
   (maximum $ map fst ps, maximum $ map snd ps))
  
renderPath :: [Pos] -> Array Pos Char
renderPath ps =
  array (pathBounds ps) (defaults ++ zip ps (repeat '#'))
  where defaults = zip (range (pathBounds ps)) (repeat '.')
  
neighbours :: Array Pos Char -> Pos -> [Pos]
neighbours a = filter (inRange (bounds a)) . possibilities
    where possibilities (x, y) = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
  
edgePoints :: Array Pos Char -> [Pos]
edgePoints a = top ++ bottom ++ left ++ right
  where ((x1, y1), (x2, y2)) = bounds a
        top = [(x,y1) | x <- [x1..x2]]
        bottom = [(x,y2) | x <- [x1..x2]]
        left = [(x1,y) | y <- [y1..y2]]
        right = [(x2,y) | y <- [y1..y2]]

-- Given a point (x, y) in the compressed coordinate system,
-- calculate the area from it to (x+1, y+1) when mapped back
-- to the original coordinate system.
areaForPoint :: Axis -> Axis -> Pos -> Int
areaForPoint xa ya (x, y) =
  (x2 - x1) * (y2 - y1)
  where x1 = xa ! x
        x2 = xa ! (x+1)
        y1 = ya ! y
        y2 = ya ! (y+1)
  
-- Calculate the total area of the bounds of a path  
totalArea :: [Pos] -> Int
totalArea ps = (x2 - x1 + 1) * (y2 - y1 + 1)
  where ((x1,y1), (x2,y2)) = pathBounds ps
  
-- Calculate the outside area from a path rendered to an array.
-- Recursively calculate the area from all edge points, marking
-- points as visited as we go. The x and y axes are used to map
-- back to the original coordinate system.
outsideArea :: Array Pos Char -> Axis -> Axis -> Int
outsideArea a xa ya =
  runST $ do
  vm <- S.newArray (bounds a) False :: ST s (S.STArray s Pos Bool)
  sum <$> mapM (flood vm) (edgePoints a)
  where
    flood vm p = do
      if (a ! p) == '#' then return 0 else do
        visited <- S.readArray vm p
        if visited then return 0 else do
          S.writeArray vm p True
          ns <- mapM (flood vm) (neighbours a p)
          return (sum ns + areaForPoint xa ya p)
   
parse :: [String] -> [Command]
parse = map parseCommand
  
part1 :: [Command] -> Int
part1 input = 
  let p = buildPath input in
  let (compressedPath, xaxis, yaxis) = compressPath p in
  let a = renderPath (joinPoints compressedPath) in
  totalArea p - outsideArea a xaxis yaxis

part2 :: [Command] -> Int
part2 = part1 . map fixCommand
  
run = Aoc.run 18 parse part1 part2

