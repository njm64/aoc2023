module Day10 where
import qualified Aoc
import Data.Array
import Util

type Coord = (Int, Int)
type PipeMap = Array Coord Char

parse :: [String] -> PipeMap 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

pipeLinks :: Char -> Coord -> [Coord]
pipeLinks c (x, y) = case c of
  '|' -> [(x, y-1), (x, y+1)]
  '-' -> [(x-1, y), (x+1, y)]
  'L' -> [(x, y-1), (x+1, y)]
  'J' -> [(x, y-1), (x-1, y)]
  '7' -> [(x-1, y), (x, y+1)]
  'F' -> [(x+1, y), (x, y+1)]
  _   -> []
  
neighbours :: PipeMap -> Coord -> [Coord]
neighbours m (x, y) = [(x', y') | x' <- [x-1..x+1],
                                  y' <- [y-1..y+1],
                                  inRange (bounds m) (x', y'),
                                  (x,y) /= (x', y')]
  
startPos :: PipeMap -> Coord
startPos m = head [i | (i,c) <- assocs m, c == 'S']

-- Return the first neighbour of s who has a pipeLink equal to s
firstLink :: PipeMap -> Coord -> Coord
firstLink m s = head . filter (elem s . links) $ neighbours m s
  where links c = pipeLinks (m ! c) c

pipeCoords :: PipeMap -> [Coord]
pipeCoords m =
  go [firstLink m start, start]
  where
    start = startPos m
    go (p:p':ps) =
      if p == start then p:p':ps
      else go (filter (/= p') (pipeLinks (m ! p) p) ++ (p:p':ps))

removeJunk :: PipeMap -> PipeMap
removeJunk m =
  let blankMap = array (bounds m) [(i, '.') | i <- indices m] in
  let pipeIndices = pipeCoords m in
  let pipeValues = map (m !) pipeIndices in
  blankMap // zip pipeIndices pipeValues

mapLine :: PipeMap -> Int -> String
mapLine m y =
  let width = fst (snd (bounds m)) in
  [m ! (x, y) | x <- [1..width]]

mapLines :: PipeMap -> [String]
mapLines m =
  let height = snd (snd (bounds m)) in
  map (mapLine m) [1..height]
  
-- Transform one line of the map. We want to count the number
-- of vertical bars, as these are in/out transitions.
-- L7 and FJ are equivalent to vertical bars.
-- LJ and F7 don't change the in/out state, so can be removed.
-- Horizontal bars can be removed for the same reason.
transformLine :: Char -> String -> String
transformLine startTile =
  replace "LJ" "" .
  replace "F7" "" .
  replace "L7" "|" .
  replace "FJ" "|" .
  replace "-" "" .
  replace "S" [startTile]
  
-- Count the number of inside tiles. The line has been preprocessed
-- by transformLine, so we just need to toggle the inside flag
-- whenever we encounter a vertical bar, and count the number of
-- dots when inside is True.
countLine :: String -> Int
countLine s =
  go s 0 False
  where
    go [] count inside = count
    go ('.':ss) count True = go ss (count + 1) True
    go ('|':ss) count inside = go ss count (not inside)
    go (_:ss) count inside = go ss count inside
  
-- Find the real start tile.
-- Try all the possibilities, and find the one whose links match
-- the coordinates of the second and second last tiles in the pipe.
startPipe :: PipeMap -> Char
startPipe m =
  let coords = pipeCoords m in
  let p1 = head (tail coords) in
  let p2 = last (init coords) in
  head [p | p <- "-LJ7F", let links = pipeLinks p (head coords),
                              p1 `elem` links,
                              p2 `elem` links]
  
part1 m = length (pipeCoords m) `div` 2

part2 m =
  let s = startPipe m in
  sum . map (countLine . transformLine s) . mapLines . removeJunk $ m

run = Aoc.run 10 parse part1 part2
