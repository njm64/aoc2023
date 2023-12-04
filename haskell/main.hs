import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

runDay :: Int -> IO ()
runDay 1 = Day1.run
runDay 2 = Day2.run
runDay 3 = Day3.run
runDay 4 = Day4.run

main :: IO ()
main = do
  runDay 4
