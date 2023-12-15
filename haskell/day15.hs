module Day15 where
import qualified Aoc
import Data.Char
import Data.Array
import Data.List
import Util

data HashOp = HashSet String Int | HashDel String deriving Show
type HashMap = Array Int [HashEntry]
type HashEntry = (String, Int)
  
tableSize = 256
  
parse :: [String] -> [String]
parse = splitList ',' . head

parseOp :: String -> HashOp
parseOp s
  | '=' `elem` s = let (k, v) = splitPair '=' s in HashSet k (read v)
  | last s == '-' = HashDel (init s)
  | otherwise = error "Invalid instruction"
  
hash :: String -> Int
hash = foldl (\acc c -> (acc + ord c) * 17 `mod` tableSize) 0 

hashNew :: HashMap
hashNew = array (0, tableSize - 1) [(i, []) | i <- [0..tableSize-1]]
  
hashSet :: HashMap -> String -> Int -> HashMap
hashSet m k v =
  let i = hash k in
  let bucket = m ! i in
  let replace (k', v') = (k', if k' == k then v else v') in
  case lookup k bucket of
    Just _ -> m // [(i, map replace bucket)]
    Nothing -> m // [(i, (k, v):bucket)]
    
hashDel :: HashMap -> String -> HashMap
hashDel m k = 
  let i = hash k in
  let bucket = [e | e <- m ! i, fst e /= k] in
  m // [(i, bucket)]
  
hashApply :: HashMap -> HashOp -> HashMap
hashApply m (HashSet k v) = hashSet m k v
hashApply m (HashDel k) = hashDel m k
  
bucketScore :: [HashEntry] -> Int
bucketScore b =
  let values = map snd $ reverse b in
    sum [i * v | (i, v) <- zip values [1..]]
  
score :: HashMap -> Int
score m = sum [(i + 1) * bucketScore b | (i, b) <- assocs m]
  
part1 = sum . map hash
part2 = score . foldl' hashApply hashNew . map parseOp
run = Aoc.run 15 parse part1 part2
