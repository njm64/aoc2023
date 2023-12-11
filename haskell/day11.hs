module Day11 where
import Data.List
import Data.Array
import qualified Aoc

type Coord = (Int, Int)
type GalaxyMap = Array Coord Char

parse :: [String] -> GalaxyMap
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

galaxies :: GalaxyMap -> [Coord]
galaxies m = [i | (i,c) <- assocs m, c == '#']
  
pairs :: (Eq a) => [a] -> [(a,a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys, x /= y]

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1,y1) (x2,y2) =
  abs (x2-x1) + abs (y2-y1)

xoffsets :: GalaxyMap -> Int -> [Int]
xoffsets m expansionSize =
  [if isClear x then expansionSize else 0 | x <- [1..w]]
  where (_, (w, h)) = bounds m
        isClear x = all (\y -> m ! (x,y) == '.') [1..h]

yoffsets :: GalaxyMap -> Int -> [Int]
yoffsets m expansionSize =
  [if isClear y then expansionSize else 0 | y <- [1..h]]
  where (_, (w, h)) = bounds m
        isClear y = all (\x -> m ! (x,y) == '.') [1..w]

mapCoord :: [Int] -> [Int] -> Coord -> Coord
mapCoord xs ys (x, y) = (x + sum (take x xs), y + sum (take y ys))

calc expansionSize input =
  let xs = xoffsets input expansionSize in
  let ys = yoffsets input expansionSize in
  let gs = map (mapCoord xs ys) (galaxies input) in
  sum $ map (uncurry manhattanDistance) (pairs gs)
  
part1 = calc 1
part2 = calc 999999
run = Aoc.run 11 parse part1 part2
