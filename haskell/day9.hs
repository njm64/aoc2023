module Day9 where
import qualified Aoc

parse :: [String] -> [[Int]]
parse = map (map read . words)
  
next :: [Int] -> Int
next xs
  | all (== 0) xs = 0
  | otherwise = last xs + next [b - a | (a, b) <- zip xs (tail xs)]
 
part1 = sum . map next
part2 = sum . map (next . reverse)
run = Aoc.run 9 parse part1 part2
