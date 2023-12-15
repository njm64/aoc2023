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


runDay :: Int -> IO ()
runDay 1 = Day1.run
runDay 2 = Day2.run
runDay 3 = Day3.run
runDay 4 = Day4.run
runDay 5 = Day5.run
runDay 6 = Day6.run
runDay 7 = Day7.run
runDay 8 = Day8.run
runDay 9 = Day9.run
runDay 10 = Day10.run
runDay 11 = Day11.run
runDay 12 = Day12.run
runDay 13 = Day13.run
runDay 14 = Day14.run
runDay 15 = Day15.run

main :: IO ()
main = do
  runDay 15
