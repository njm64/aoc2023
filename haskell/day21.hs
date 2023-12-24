module Day21 where
import qualified Aoc
import Data.Array 
import qualified Data.Array.ST as S
import Control.Monad.ST
import Control.Monad
import Util

type Pos = (Int, Int)
type Garden = Array Pos Char

parse :: [String] -> Garden 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = concat lines in
  array ((1,1), (width, height)) (zip coords cells)

startPos :: Garden -> Pos
startPos garden = head [pos | (pos, c) <- assocs garden, c == 'S']

neighbours :: Garden -> Pos -> [Pos]
neighbours a = filter (inRange (bounds a)) . possibilities
    where possibilities (x, y) = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

search :: Garden -> Pos -> Int -> Int
search g start steps =
  runST $ do
  vm <- S.newArray (bounds g) [] :: ST s (S.STArray s Pos [Int])
  go vm 0 start
  elems <- S.getElems vm
  return (length . filter (steps `elem`) $ elems)
  where
    go vm n p = do
      unless (g ! p == '#') $ do
        costs <- S.readArray vm p
        unless (n `elem` costs) $ do
          S.writeArray vm p (n : costs)
          when (n < steps) $ do
            mapM_ (go vm (n + 1)) (neighbours g p)
  
part1 garden =
  let start = startPos garden in
  search garden start 64
    
part2 garden = 0

run = Aoc.run 21 parse part1 part2

