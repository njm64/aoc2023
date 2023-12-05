module Day5 where
import Util
import Data.List
import qualified Aoc

-- A range is a pair of inclusive minimum and maximum values
type Range = (Int, Int) 

-- A mapping entry is a range, and a delta to apply to values in this range
type MapEntry = (Range, Int)
type Map = [MapEntry]

parseMapEntry :: String -> MapEntry
parseMapEntry s = 
  let lst = map read $ words s in
  let [dst,src,len] = lst in
  ((src, src + len - 1), dst - src)
    
parse :: [String] -> ([Int], [Map])
parse lines = 
  let (x:xs) = splitWith null lines in 
  let seeds = map read . tail . words . head $ x in
  let maps = map (map parseMapEntry . tail) xs in
  (seeds, maps)
  
mapValue :: Int -> Map -> Int
mapValue n m = case find (\(r,_) -> inRange r n) m of
    Just (_, delta) -> n + delta
    Nothing -> n
    
inRange :: Range -> Int -> Bool
inRange (a, b) n = n >= a && n <= b
  
-- Calculate the intersection between two ranges, and
-- return a list of ranges that are in both. 
intersectRanges :: Range -> Range -> [Range]
intersectRanges (a1, a2) (b1, b2)
  | b2 < a1 = []                      -- b1..b2..a1..a2
  | a2 < b1 = []                      -- a1..a2..b1..b2
  | a1 <= b1 && b2 <= a2 = [(b1,b2)]  -- a1..b1..b2..a2
  | b1 <= a1 && a2 <= b2 = [(a1,a2)]  -- b1..a1..a2..b2 
  | a1 < b1 = [(b1,a2)]               -- a1..b1..a2..b2
  | a2 > b2 = [(a1,b2)]               -- b1..a1..b2..a2

-- Subtract b from a, and return a list of ranges
-- (i.e. the parts of a that are outside b)
subtractRange :: Range -> Range -> [Range]
subtractRange (a1, a2) (b1, b2)
  | b2 < a1 = [(a1, a2)]                          -- b1..b2..a1..a2
  | a2 < b1 = [(a1, a2)]                          -- a1..a2..b1..b2
  | a1 <= b1 && b2 <= a2 = [(a1,b1-1),(b2+1,a2)]  -- a1..b1..b2..a2
  | b1 <= a1 && a2 <= b2 = []                     -- b1..a1..a2..b2 
  | a1 < b1 = [(a1,b1-1)]                         -- a1..b1..a2..b2
  | a2 > b2 = [(b2+1,a2)]                         -- b1..a1..b2..a2
  
-- Apply a map entry to a range r. It's possible that only part (or none)
-- of the range will be mapped, so return a tuple with the mapped and
-- unmapped parts of the original range.
mapRange :: MapEntry -> Range -> ([Range], [Range])
mapRange (mrange, delta) r =
  let mappedRanges = intersectRanges r mrange in
  let unmappedRanges = subtractRange r mrange in
  let applyDelta (a, b) = (a + delta, b + delta) in
  (map applyDelta mappedRanges, unmappedRanges)
      
-- Apply a map to a range. Basically we apply each mapping entry in turn
-- to the range. This will split it into a list of mapped ranges and a
-- list of unmapped ranges. Keep trying to map the unmapped ones against
-- subsequent mapping entries.
mapRanges :: [Range] -> Map -> [Range]
mapRanges rs [] = rs
mapRanges rs (e:es) =
  let lst = map (mapRange e) rs in
  let mapped = concatMap fst lst in
  let unmapped = concatMap snd lst in
  mapped ++ mapRanges unmapped es
  
seedsToRanges :: [Int] -> [Range]
seedsToRanges = map (\[a,b] -> (a, a+b-1)) . chunks 2 
  
part1 (seeds, maps) =
  minimum $ map f seeds 
  where f n = foldl' mapValue n maps
  
part2 (seeds, maps) =
  let ranges = seedsToRanges seeds in
  minimum . map fst $ foldl' mapRanges ranges maps

run = Aoc.run 5 parse part1 part2

