module Day24 where
import qualified Aoc
import Util
import Data.Maybe
import Data.List
import Data.Ix
import qualified Data.Map as Map
import Control.Monad  

type Vec3 = (Integer, Integer, Integer)
type Hailstone = (Vec3, Vec3)
type AxisFn = Vec3 -> Integer

vecX (x, _, _) = x
vecY (_, y, _) = y
vecZ (_, _, z) = z
  
parseVec3 :: String -> Vec3
parseVec3 s =
  let [x, y, z] = map read . words $ replace "," "" s in
  (x, y, z)
  
parse :: [String] -> [Hailstone]
parse = map parseLine
  where parseLine s =
          let (p, v) = splitPair '@' s in
          (parseVec3 p, parseVec3 v)
  
intersect2D :: Hailstone -> Hailstone -> Maybe Vec3
intersect2D ((x1, y1, _), (dx1, dy1, _)) ((x2, y2, _), (dx2, dy2, _)) =
  case dx1 * dy2 - dy1 * dx2 of
    0 -> Nothing
    d -> Just ((q * dx1 - p * dx2) `div` d, (q * dy1 - p * dy2) `div` d, 0)
         where p = x1 * (y1 + dy1) - y1 * (x1 + dx1)
               q = x2 * (y2 + dy2) - y2 * (x2 + dx2)
    
timeUntilIntersection :: Hailstone -> Vec3 -> Integer
timeUntilIntersection ((x, y, _), (dx, dy, _)) (ix, iy, _)
  | dx /= 0 = (ix - x) `div` dx
  | dy /= 0 = (iy - y) `div` dy
  | otherwise = error "No intersection"
  
intersectFuture :: Hailstone -> Hailstone -> Maybe Vec3
intersectFuture a b =
  mfilter (\i -> inFuture a i && inFuture b i) $ a `intersect2D` b
  where inFuture h i = timeUntilIntersection h i >= 0
  
part1 :: [Hailstone] -> Int
part1 hs =
  length . filter inTestArea $ catMaybes intersections
  where
    intersections = [a `intersectFuture` b | (a:as) <- tails hs, b <- as]
    r = (200000000000000, 400000000000000) 
    inTestArea (x, y, _) = inRange r x && inRange r y

-- Find hailstones with the same velocity along an axis, and group
-- them together. Returns a list of (velocity, [position]) pairs.
groupv :: AxisFn -> [Hailstone] -> [(Integer, [Integer])]
groupv axis hs =
  Map.toList . Map.filter multiple . Map.fromListWith (++) $ map toScalars hs
  where toScalars (p, v) = (axis v, [axis p])
        multiple vs = length vs > 1

-- Find the rock velocity along the given axis.
-- This clever algorithm comes from here:
-- https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/keqf8uq/
findv :: AxisFn -> [Hailstone] -> Integer
findv axis hs =
  head . foldl1 intersect . map candidates $ groupv axis hs
  where candidates (hv, p1:p2:_) =
          [rv | rv <- [-1000..1000], rv /= hv, (p1 - p2) `mod` (rv - hv) == 0]

subVelocity :: Vec3 -> Hailstone -> Hailstone
subVelocity (rx, ry, rz) (pos, (hx, hy, hz)) = 
  (pos, (hx - rx, hy - ry, hz - rz))
  
rockPosition :: [Hailstone] -> Vec3 -> Vec3
rockPosition hs rv =
  let [a, b] = map (subVelocity rv) (take 2 hs) in
  let (rx, ry, _) = fromJust $ intersect2D a b in
  let ((ax, _, az), (axv, _, azv)) = a in
  let t = (rx - ax) `div` axv in
  let rz = az + t * azv in
  (rx, ry, rz)
  
part2 :: [Hailstone] -> Int
part2 hs =
  let rv = (findv vecX hs, findv vecY hs, findv vecZ hs) in
  let (x, y, z) = rockPosition hs rv in
  fromInteger $ x + y + z
    
run = Aoc.run 24 parse part1 part2
