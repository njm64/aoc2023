module Day12 where
import Data.List
import Data.Array
import Data.Maybe
import Util
import qualified Aoc

type SpringRow = (String, [Int])

parseLine :: String -> SpringRow
parseLine s =
  let (lhs, rhs) = splitPair ' ' s in
  (lhs, map read (splitList ',' rhs))
  
parse :: [String] -> [SpringRow]
parse = map parseLine

expandRecord :: SpringRow -> SpringRow
expandRecord (s, ns) =
  (intercalate "?" $ replicate 5 s, concat $ replicate 5 ns)

removeBroken :: String -> Int -> Maybe String
removeBroken xs n =
  let (broken, rest) = splitAt n xs in
  if length broken < n || '.' `elem` broken then Nothing
  else case rest of
    ""       -> Just ""
    ('#':_)  -> Nothing
    (_:xs) -> Just xs

-- TODO: This is a work in progress. It works, but too slowly for part 2.
  
count :: String -> [Int] -> Int
-- Normal termination condition
count [] [] = 1
count [] (_:_) = 0
-- No integers left, but there is a broken spring. Not ok.
count ('#':_) [] = 0
-- If the first char is a dot, we can ignore it.
count ('.':xs) ns = count xs ns
-- If the first char is a ? and no integers left, treat it like a dot
count ('?':xs) [] = count xs []
-- If the first character is a #, check to see if there are n
-- consecutive broken springs followed by an unbroken spring. If
-- so, remove them and recurse. If not, the match fails.
count ('#':xs) (n:ns) =
  case removeBroken ('#':xs) n of
    Just xs -> count xs ns
    Nothing -> 0
-- If the first character is unknown, check to see if there could
-- be n consecutive broken springs followed by an unbroken spring.
-- If so, there are two possibilities. The spring could be broken
-- or unbroken, so recurse twice and add the counts. If it doesn't
-- match, then only recurse for the unbroken case.
count ('?':xs) (n:ns) =
  case removeBroken ('?':xs) n of
    Just xs' -> count xs (n:ns) + count xs' ns
    Nothing -> count xs (n:ns)

part1 = sum . map (uncurry count)
part2 = sum . map (uncurry count . expandRecord) 
run = Aoc.run 12 parse part1 part2
