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
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in 
  array ((1,1), (height, width)) (zip coords cells)
  
neighbours :: EngineMap -> NumberDef -> [Coord]
neighbours m ((x, y), num) =
  let len = length num in
  let numBounds = ((x, y), (x+len-1, y)) in
  [(nx, ny) | nx <- [x-1..x+len],
              ny <- [y-1..y+1],
              inRange (bounds m) (nx, ny),
              not (inRange numBounds (nx, ny))]
  
numbers :: EngineMap -> [NumberDef]
numbers m = map combine . groupBy' cmp $ digits 
  where ix = sortOn (\(a, b) -> (b, a)) (indices m)
        digits = [(i, d) | i <- ix, let d = m ! i, isDigit d]
        cmp ((x1,y1),_) ((x2,y2),_) = y1 == y2 && x2 == x1 + 1
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

