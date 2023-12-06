module Day6 where
import qualified Aoc

type Race = (Int, Int)
  
parse :: [String] -> [Race]
parse lines =
  let times = map read . tail . words . head $ lines in
  let distances = map read . tail . words $ lines !! 1 in
  zip times distances

wins :: Race -> Int
wins (time, distance) =
  length [d | speed <- [0..time],
              let d = (time - speed) * speed,
              d > distance]
  
combineNums :: [Race] -> Race
combineNums races =
  let time = read . concatMap (show . fst) $ races in
  let distance = read . concatMap (show . snd) $ races in
  (time, distance)

part1 = product . map wins
part2 = wins . combineNums
run = Aoc.run 6 parse part1 part2

