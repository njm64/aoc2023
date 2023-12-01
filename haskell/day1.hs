module Day1 where
import Data.Char
import Util
import qualified Aoc

nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
mappings = [(show i, i) | i <- [1..9]] ++ zip nums [1..]

firstDigit :: String -> Int
firstDigit s =
  let find = position s in
  snd . minimum $ [(i, d) | (name, d) <- mappings, let i = find name, i >= 0]
    
lastDigit :: String -> Int
lastDigit s =
  let find = position (reverse s) . reverse in
  snd . minimum $ [(i, d) | (name, d) <- mappings, let i = find name, i >= 0]

calc1 line = let digits = filter isDigit line in
  read [head digits, last digits]

calc2 line = firstDigit line * 10 + lastDigit line

part1 = sum . map calc1 
part2 = sum . map calc2 
run = Aoc.run 1 id part1 part2

