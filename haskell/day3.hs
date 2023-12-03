module Day3 where
import Data.Char
import Data.Array
import Data.List
import Util
import qualified Aoc

type Coord = (Int, Int)
type EngineMap = Array Coord Char
type NumberDef = (Coord, String)  

parse :: [String] -> EngineMap
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(y,x) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in 
  array ((1,1), (height, width)) (zip coords cells)
  
neighbours :: EngineMap -> NumberDef -> [Coord]
neighbours m ((y, x), num) =
  let len = length num in
  let numBounds = ((y, x), (y, x+len-1)) in
  [(ny, nx) | nx <- [x-1..x+len],
              ny <- [y-1..y+1],
              inRange (bounds m) (ny, nx),
              not (inRange numBounds (ny, nx))]
  
numbers :: EngineMap -> [NumberDef]
numbers m = map combine . groupBy' cmp $ digits 
  where digits = [(i, d) | i <- indices m, let d = m ! i, isDigit d]
        cmp ((y1,x1),_) ((y2,x2),_) = y1 == y2 && x2 == x1 + 1
        combine ds = (fst . head $ ds, map snd ds)  
  
isEnginePart :: EngineMap -> NumberDef -> Bool
isEnginePart m n = any (isSym . (m !)) $ neighbours m n
  where isSym c = not (isDigit c) && c /= '.'
  
gearPositions :: EngineMap -> NumberDef -> [(Coord, NumberDef)]
gearPositions m n = [(pos, n) | pos <- neighbours m n, m ! pos == '*']
  
groupByGears :: EngineMap -> [NumberDef] -> [[NumberDef]]
groupByGears m n = map (map snd) . filter (\g -> length g == 2) .
  groupOn fst . sort $ concatMap (gearPositions m) n

gearRatio :: [NumberDef] -> Int
gearRatio = product . map (read . snd) 
  
part1 m = sum . map (read . snd) . filter (isEnginePart m) $ numbers m
part2 m = sum . map gearRatio . groupByGears m $ numbers m
run = Aoc.run 3 parse part1 part2

