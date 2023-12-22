import System.Environment
import Text.Read
import Data.Maybe
import qualified Day1 
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20

runners = [
  Day1.run, Day2.run, Day3.run, Day4.run, Day5.run,
  Day6.run, Day7.run, Day8.run, Day9.run, Day10.run,
  Day11.run, Day12.run, Day13.run, Day14.run, Day15.run,
  Day16.run, Day17.run, Day18.run, Day19.run, Day20.run]
  
parseInt :: String -> Maybe Int
parseInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing
  
runnerForDay :: Int -> Maybe (IO ())
runnerForDay n = listToMaybe $ drop (n - 1) runners
  
main :: IO ()
main = do
  args <- getArgs
  if null args then
    sequence_ runners
  else 
    case parseInt (head args) >>= runnerForDay of
      Just r -> r
      _ -> putStrLn "Usage: aoc <day>"
