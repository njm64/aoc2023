module Day21 where
import qualified Aoc
import Data.Array 
import qualified Data.Array.ST as S
import Control.Monad.ST
import Control.Monad

type Pos = (Int, Int)
type Garden = Array Pos Char

parse :: [String] -> Garden 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

-- Just use the centre location for the start position, so that
-- this works with expanded gardens.
startPos :: Garden -> Pos
startPos garden =
  let (w, h) = snd $ bounds garden in
  (w `div` 2 + 1, h `div` 2 + 1)

neighbours :: Garden -> Pos -> [Pos]
neighbours a = filter (inRange (bounds a)) . possibilities
    where possibilities (x, y) = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

-- Recursively search for reachable plots
search :: Int -> Garden -> Array Pos Bool
search steps g =
  runST $ do
  vm <- S.newArray (bounds g) [] :: ST s (S.STArray s Pos [Int])
  go vm 0 (startPos g)
  fmap (steps `elem`) <$> S.freeze vm
  where
    go vm n p = do
      unless (g ! p == '#') $ do
        costs <- S.readArray vm p
        unless (n `elem` costs) $ do
          S.writeArray vm p (n : costs)
          when (n < steps) $ do
            mapM_ (go vm (n + 1)) (neighbours g p)
  
-- Expand a garden, making it n times bigger in both directions
expandGarden :: Garden -> Int ->  Garden
expandGarden g n =
  array ((1, 1), (ew, eh)) [((x, y), f x y) | x <- [1..ew], y <- [1..eh]]
  where (w, h) = snd $ bounds g
        (ew, eh) = (w * n, h * n)
        f x y = g ! ((x - 1) `mod` w + 1, (y - 1) `mod` h + 1)
  
-- Given an array of reachable garden plot positions, summarise it
-- into counts for each tile, where n is the number of tiles on each axis. 
makeTiles :: Array Pos Bool -> Int -> Array Pos Int
makeTiles a n =
  array ((1, 1), (n, n)) [((x, y), f x y) | x <- [1..n], y <- [1..n]]
  where
    (w, h) = snd $ bounds a
    (tw, th) = (w `div` n, h `div` n)
    f tx ty = length [1 | x <- [1..tw], y <- [1..th],
                          a ! ((tx - 1) * tw + x, (ty - 1) * th + y)]
                      
-- The key to solving this puzzle was visualising the data. When searching
-- an expanded garden, almost every second plot within a diamond shaped
-- region was reachable (unless the path to the plot was blocked by
-- # characters). Running searches on gardens of increasing sizes, and
-- summarising the reachable plot counts per tile reveals the pattern
-- below:

-- 3x3 garden
----------------
--  959 5587 938
-- 5563 7388 5593
--  948 5569 949

-- 5x5 garden
---------------
--    0  959 5587  938    0
--  959 6492 7388 6496  938
-- 5563 7388 7401 7388 5593
--  948 6472 7388 6498  949
--    0  948 5569  949    0

-- 7x7 garden
---------------
--    0    0  959 5587  938    0    0
--    0  959 6492 7388 6496  938    0
--  959 6492 7388 7401 7388 6496  938
-- 5563 7388 7401 7388 7401 7388 5593
--  948 6472 7388 7401 7388 6498  949
--    0  948 6472 7388 6498  949    0
--    0    0  948 5569  949    0    0

-- 9x9 garden
---------------
--    0    0    0  959 5587  938    0    0    0
--    0    0  959 6492 7388 6496  938    0    0
--    0  959 6492 7388 7401 7388 6496  938    0
--  959 6492 7388 7401 7388 7401 7388 6496  938
-- 5563 7388 7401 7388 7401 7388 7401 7388 5593
--  948 6472 7388 7401 7388 7401 7388 6498  949
--    0  948 6472 7388 7401 7388 6498  949    0
--    0    0  948 6472 7388 6498  949    0    0
--    0    0    0  948 5569  949    0    0    0

-- So we just need to multiply out these counts for the target
-- number of tile repetitions. A 5x5 tile map gives us
-- enough information to extrapolate up to a given tile count n:
count :: Array Pos Int -> Int -> Int
count m n =
  -- Diagonal edges
  m ! (2, 1) * n +
  m ! (4, 1) * n +
  m ! (2, 5) * n +
  m ! (4, 5) * n +
  -- Inner diagonal edges
  m ! (2, 2) * (n - 1) +
  m ! (4, 2) * (n - 1) +
  m ! (2, 4) * (n - 1) +
  m ! (4, 4) * (n - 1) +
  -- Top/bottom/left/right corners
  m ! (3, 1) +
  m ! (3, 5) +
  m ! (1, 3) +
  m ! (5, 3) +
  -- Middle
  m ! (3, 2) * n * n +
  m ! (3, 3) * (n - 1) * (n - 1)
  
part1 = length . filter id . elems . search 64 
    
part2 garden =
  -- First expand the garden to 5 times its size in both directions
  let bigGarden = expandGarden garden 5 in
  -- Next, search for reachable plots in the expanded garden. The
  -- search size 327 is half the width/height of the garden, rounded down.
  let plots = search 327 bigGarden in
  -- Build a map of counts in each of the 5x5 tiles.
  let tileMap = makeTiles plots 5 in
  -- Now multiply out the counts to calculate the total.
  -- The target number of steps is 26501365, and the tile width/height is
  -- 131. It takes 65 steps to reach the edge of this tile, and then we
  -- have repeated tiles on each side of it. The number of repeated tiles
  -- is (26501365 - 65) / 131, which works out to 202300.
  count tileMap 202300

run = Aoc.run 21 parse part1 part2

