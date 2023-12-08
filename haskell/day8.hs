module Day8 where
import qualified Aoc
import Data.Maybe

type Node = (String, (String, String))
type GoalFn = (String -> Bool)
type Step = Char

parseNode line =
  let node = take 3 line in
  let left = take 3 (drop 7 line) in
  let right = take 3 (drop 12 line) in
  (node, (left, right))
  
parse lines =
  let rules = head lines in
  let nodes = map parseNode (drop 2 lines) in
  (rules, nodes)

solve :: GoalFn -> [Step] -> [Node] -> String -> Int
solve isGoal steps nodes start =
  go (cycle steps) start 0
  where go (s:steps) node count
          | isGoal node = count
          | otherwise = go steps next (count + 1)
          where (left, right) = fromJust (lookup node nodes)
                next = if s == 'L' then left else right
  
solve1 = solve (== "ZZZ")
solve2 = solve (\s -> last s == 'Z')
  
part1 (steps, nodes) = solve1 steps nodes "AAA" 

part2 (steps, nodes) =
  let startNodes = [n | n <- map fst nodes, last n == 'A'] in
  let cycleLengths = map (solve2 steps nodes) startNodes in
  foldr lcm 1 cycleLengths
  
run = Aoc.run 8 parse part1 part2
