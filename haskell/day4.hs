module Day4 where
import Data.Char
import Data.List
import Util
import qualified Aoc

type Card = (Int, [Int], [Int])
  
parseNums :: String -> [Int]
parseNums = map read . words

parseCard :: String -> Card
parseCard s =
  let (lhs, rhs) = splitPair ':' s in
  let id = read $ words lhs !! 1 in
  let (lnums, rnums) = splitPair '|' rhs in
  (id, parseNums lnums, parseNums rnums)
  
parse :: [String] -> [Card]
parse = map parseCard

matches :: Card -> Int
matches (_, a, b) = length (a `intersect` b)

score :: Card -> Int
score card = 
  let n = matches card in
  if n == 0 then 0 else 2 ^ (n - 1)
  
-- Given the list of match counts for a card and subsequent cards,
-- return the number of matches for the card at the head of the list.
countCards :: [Int] -> Int
countCards [] = 0
countCards (x:xs) = 1 + (sum . map countCards . take x $ tails xs)
  
part1 = sum . map score 
part2 = sum . map countCards . tails . map matches
run = Aoc.run 4 parse part1 part2
