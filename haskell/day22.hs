module Day22 where
import qualified Aoc
import Data.Array
import Data.List
import Util

type Pos = (Int, Int, Int)
type Brick = (Pos, Pos)
type BrickMap = Array Pos Bool

parsePos :: String -> Pos
parsePos s = case map read $ splitList ',' s of
  [x, y, z] -> (x, y, z)

parseBrick :: String -> Brick
parseBrick s =
  let (a, b) = splitPair '~' s in
  (parsePos a, parsePos b)

parse :: [String] -> [Brick]
parse = map parseBrick

posX (x, _, _) = x
posY (_, y, _) = y
posZ (_, _, z) = z
  
irange :: Int -> Int -> [Int]  
irange a b
  | a < b = [a..b]
  | otherwise = [a,a-1..b]
  
-- Return a list of all positions occupied by a brick
brickPositions :: Brick -> [Pos]
brickPositions ((x1, y1, z1), (x2, y2, z2)) =
  [(x, y, z) | x <- irange x1 x2, y <- irange y1 y2, z <- irange z1 z2]

-- Return a list of all positions directly below a brick
positionsBelowBrick :: Brick -> [Pos]
positionsBelowBrick ((x1, y1, z1), (x2, y2, z2)) =
  [(x, y, min z1 z2 - 1) | x <- irange x1 x2, y <- irange y1 y2]

-- Determine the bounds for creating a brick map. Note we use zero for
-- the minimum z bound, so that we can check that positions below the
-- bottom brick are clear.
brickMapBounds :: [Brick] -> (Pos, Pos)
brickMapBounds bs =
  let ps = concatMap (\(a, b) -> [a, b]) bs in
  ((minimum $ map posX ps, minimum $ map posY ps, 0),
   (maximum $ map posX ps, maximum $ map posY ps, maximum $ map posZ ps))

-- Build a map to indicate which positions are occupied.
buildBrickMap :: [Brick] -> BrickMap
buildBrickMap bs =
  a // [(p, True) | p <- concatMap brickPositions bs]
  where a = arrayWithDefault (brickMapBounds bs) False
  
canDropBrick :: BrickMap -> Brick -> Bool
canDropBrick bm b@((_, _, z1), (_, _, z2))
  | min z1 z2 <= 1 = False
  | otherwise = not $ any (bm !) (positionsBelowBrick b)
  
dropBrick :: Brick -> Brick
dropBrick ((x1, y1, z1), (x2, y2, z2)) =
  ((x1, y1, z1-1), (x2, y2, z2-1))
  
stepGravity :: [Brick] -> [Brick]
stepGravity bs =
  map f bs
  where bm = buildBrickMap bs
        f b | canDropBrick bm b = dropBrick b
            | otherwise = b

runGravity :: [Brick] -> [Brick]
runGravity bs =
  let bs' = stepGravity bs in
  if bs == bs' then bs
  else runGravity bs'

canRemoveBrick :: [Brick] -> Brick -> Bool
canRemoveBrick bs b =
  let bs' = delete b bs in
  bs' == stepGravity bs'
  
getDropCount :: [Brick] -> Brick -> Int
getDropCount bs b =
  let bs1 = delete b bs in
  let bs2 = runGravity bs1 in
  length [1 | (a, b) <- zip bs1 bs2, a /= b]
  
part1 bs =
  let bs' = runGravity bs in
  length $ filter (canRemoveBrick bs') bs'

part2 bs =
  let bs' = runGravity bs in
  sum $ map (getDropCount bs') bs'

run = Aoc.run 22 parse part1 part2

