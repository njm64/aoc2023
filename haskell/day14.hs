module Day14 where
import qualified Aoc
import Data.Array
import Data.List

type Coord = (Int, Int)
type RockMap = Array Coord Char
data Direction = North | South | East | West 
  
parse :: [String] -> RockMap 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

move :: Direction -> Coord -> Coord
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moveRocks :: Direction -> RockMap -> RockMap
moveRocks dir m =
  m // concat [[(pos, '.'), (newPos, 'O')] | (pos, c) <- assocs m,
                c == 'O', let newPos = move dir pos,
                inRange (bounds m) newPos, m ! newPos == '.']

tiltPlatform :: Direction -> RockMap -> RockMap
tiltPlatform dir m =
  let m' = moveRocks dir m in
  if m' == m then m else tiltPlatform dir m'
  
cycleDish :: RockMap -> RockMap
cycleDish m = foldl (flip tiltPlatform) m [North, West, South, East]
  
totalLoad :: RockMap -> Int
totalLoad m =
  let height = snd $ snd (bounds m) in
  sum [load | ((x, y), c) <- assocs m, c == 'O', let load = height - y + 1]
  
-- Return the first & second indices of the first repeating
-- element in a sequence.
findRepeat :: (Eq a) => [a] -> (Int, Int)               
findRepeat ds =
  head [(j,i) | (i,a) <- zip [0..] ds,
                (j,b) <- zip [0..i-1] ds,
                a == b]
  
part1 = totalLoad . tiltPlatform North

part2 m =
  let ds = iterate cycleDish m in
  let (start, end) = findRepeat ds in
  let n = (1000000000 - start) `mod` (end - start) in
  map totalLoad ds !! (start + n)

run = Aoc.run 14 parse part1 part2

