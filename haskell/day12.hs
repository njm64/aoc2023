module Day12 where
import Data.List
import Util
import qualified Aoc
import qualified Data.Map as Map

type SpringRow = (String, [Int])

parseLine :: String -> SpringRow
parseLine s =
  let (lhs, rhs) = splitPair ' ' s in
  (lhs, map read (splitList ',' rhs))
  
parse :: [String] -> [SpringRow]
parse = map parseLine

expandRow :: SpringRow -> SpringRow
expandRow (s, ns) =
  (intercalate "?" $ replicate 5 s, concat $ replicate 5 ns)

-- Attempt to remove n broken springs from the beginning of xs.
-- This will fail if the next spring is unbroken.
removeBroken :: String -> Int -> Maybe String
removeBroken xs n =
  let (broken, rest) = splitAt n xs in
  if length broken < n || '.' `elem` broken then Nothing
  else case rest of
    ""       -> Just ""
    ('#':_)  -> Nothing
    (_:xs) -> Just xs

-- Count the number of permutations of spring arrangements
-- Original unmemoised implementation.
-- This was fine for part 1 but too slow for part 2.
count :: String -> [Int] -> Int
-- Normal termination condition
count [] [] = 1 
-- No broken springs left, but there is a broken count
count [] _ = 0  
-- No broken counts left, but there is a broken spring
count ('#':_) [] = 0 
-- Skip unbroken springs
count ('.':xs) ns = count xs ns 
-- No broken counts left, and first spring is a wildcard.
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

-- Memoised version of count. Build a map with all
-- permutations of the tails of xs and ns. Then rely on
-- lazy evaluation to calculate the map values as required.
countMemo :: SpringRow -> Int
countMemo (xs, ns) = lookup xs ns
  where cache = Map.fromList [((xs', ns'), f xs' ns') | xs' <- tails xs, ns' <- tails ns] 
        lookup a b = cache Map.! (a, b)
        f [] [] = 1
        f [] _ = 0
        f ('#':_) [] = 0
        f ('.':xs) ns = lookup xs ns
        f ('?':xs) [] = lookup xs []
        f ('#':xs) (n:ns) =
          case removeBroken ('#':xs) n of
            Just xs -> lookup xs ns
            Nothing -> 0
        f ('?':xs) (n:ns) =
          case removeBroken ('?':xs) n of
            Just xs' -> lookup xs (n:ns) + lookup xs' ns
            Nothing -> lookup xs (n:ns)

part1 = sum . map countMemo 
part2 = sum . map (countMemo . expandRow)
run = Aoc.run 12 parse part1 part2
