module Day23 where
import qualified Aoc
import Data.Array
import Util
import Data.Maybe
import Data.List 
import Data.Bits
import qualified Data.Map as Map

import Debug.Trace
import Text.Printf
  
type Pos = (Int, Int)
type Maze = Array Pos Char

type Graph = Array Int Node
type Node = [Link]
type Link = (Int, Int)
  
parse :: [String] -> Maze 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

-- Source & destination positions
srcPos _ = (2,1)
dstPos m = let (x, y) = snd $ bounds m in (x - 1, y)

-- Get neighbours for part 1, for a given position.
-- We check that each neighbour position is in range,
-- and passable in the required direction.
neighbours :: Maze -> Pos -> [Pos]
neighbours m (x,y) =
  catMaybes [up, down, left, right]
  where
    up = maybePos (x, y-1) "^."
    down = maybePos (x, y+1) "v."
    left = maybePos (x-1, y) "<."
    right = maybePos (x+1, y) ">."
    maybePos p s
      | inRange (bounds m) p && m ! p `elem` s = Just p
      | otherwise = Nothing
        
-- Return true if the given position is walkable for part 2
isWalkable :: Maze -> Pos -> Bool
isWalkable m p = inRange (bounds m) p && m ! p `elem` ".<>^v"

-- Get neighbours for part 2
neighbours' :: Maze -> Pos -> [Pos]
neighbours' m (x,y) =
  [p | p <- [(x, y-1), (x, y+1), (x-1, y), (x+1, y)], isWalkable m p]

-- Return true if the given position is a graph node.
-- i.e. it's the source node, the goal node, or it has more than
-- 2 neighbours.
isNode :: Maze -> Pos -> Bool
isNode m p =
  isWalkable m p &&
  (p == srcPos m || p == dstPos m || length (neighbours' m p) > 2)
  
nodePoints :: Maze -> [Pos]
nodePoints m = [p | p <- indices m, isNode m p]
  
-- Given a maze and a node, return a list of adjacencies.
-- Each adjacency is the position of another node, and the cost
-- to reach that node.
adjacencies :: Maze -> Pos -> [(Pos, Int)]
adjacencies m p =
  mapMaybe (f 1 p) (neighbours' m p)
  where
    f cost prev p
      | isNode m p = Just (p, cost)
      | otherwise = 
      case [n | n <- neighbours' m p, n /= prev] of
        [] -> Nothing
        [a] -> f (cost + 1) p a 
  
-- Now build a graph. This is just an array of nodes, where each
-- node is a list of links. A link has the index of the destination node,
-- and the cost to reach it.
buildGraph :: Maze -> Graph
buildGraph m =
  array (1, length ps) (zip [1..] nodes)
  where ps = nodePoints m
        nodeMap = Map.fromList (zip ps [1..])
        nodes = map getLinks ps
        getLinks p = [(nodeMap Map.! p, cost) | (p, cost) <- adjacencies m p]
  
  
-- Part 1 is a simple recursive search.
-- We remember the previous position to avoid backtracking.
part1 :: Maze -> Int
part1 m =
  fromJust $ go (0,0) (srcPos m)
  where
    dst = dstPos m
    go prev src
      | src == dst = Just 0
      | otherwise =
        let ns = delete prev $ neighbours m src in
        case mapMaybe (go src) ns of
          [] -> Nothing
          is -> Just (maximum is + 1)

-- For part 2, we first build a graph with a node for each junction.
-- There are only around 30 of these, so use a bitmask to track which
-- ones we've visited, so we don't pass through the same junction twice.
part2 :: Maze -> Int
part2 m =
  fromJust $ go 0 (0 :: Int) 1 
  where
    g = buildGraph m
    dst = length g
    go cost mask src 
      | src == dst = Just cost
      | otherwise = 
        let mask' = setBit mask src in
        let visited link = testBit mask (fst link) in
        let links = filter (not . visited) (g ! src) in
        case mapMaybe (\(p, c) -> go (cost + c) mask' p) links of
          [] -> Nothing
          is -> Just (maximum is)
        
run = Aoc.run 23 parse part1 part2

