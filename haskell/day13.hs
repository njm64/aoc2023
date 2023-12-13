module Day13 where
import qualified Aoc
import Data.Array
import Util

type TerrainMap = Array (Int, Int) Char
  
parseMap :: [String] -> TerrainMap
parseMap lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

verticalReflection :: TerrainMap -> Int -> Bool
verticalReflection m i =
  all rowsEqual (zip [i,i-1..1] [i+1..h])
  where (w, h) = snd $ bounds m
        rowsEqual (a, b) = all (\x -> m ! (x, a) == m ! (x, b)) [1..w]

horizontalReflection :: TerrainMap -> Int -> Bool
horizontalReflection m i =
  all colsEqual (zip [i,i-1..1] [i+1..w])
  where (w, h) = snd $ bounds m
        colsEqual (a, b) = all (\y -> m ! (a, y) == m ! (b, y)) [1..h]
  
reflectionScores :: TerrainMap -> [Int]
reflectionScores m =
  let (w, h) = snd $ bounds m in
  let hs = filter (horizontalReflection m) [1..w-1] in
  let vs = filter (verticalReflection m) [1..h-1] in
  hs ++ fmap (100 *) vs
  
unsmudgeMap :: TerrainMap -> (Int, Int) -> TerrainMap  
unsmudgeMap m i = m // [(i, unsmudge $ m ! i)]
  where unsmudge '#' = '.'
        unsmudge '.' = '#'
  
unsmudgedScore :: TerrainMap -> Int
unsmudgedScore m =
  let score = head $ reflectionScores m in
  let variants = map (unsmudgeMap m) (indices m) in
  head $ concatMap (filter (/= score) . reflectionScores) variants

parse :: [String] -> [TerrainMap]
parse = map parseMap . splitWith null
  
part1 = sum . concatMap reflectionScores
part2 = sum . map unsmudgedScore
run = Aoc.run 13 parse part1 part2
