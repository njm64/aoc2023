module Day1 where
import Data.Char
import Util
import qualified Aoc

nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
mappings = [(show i, i) | i <- [1..9]] ++ zip nums [1..]

findNum f = snd . minimum $ [(i, d) | (s, d) <- mappings, let i = f s, i >= 0]
firstDigit s = findNum (position s)
lastDigit s = findNum (position (reverse s) . reverse)

calc1 line =
  let digits = filter isDigit line in
  read [head digits, last digits]

calc2 line = firstDigit line * 10 + lastDigit line

part1 = sum . map calc1 
part2 = sum . map calc2 
run = Aoc.run 1 id part1 part2

