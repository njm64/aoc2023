import qualified Day1
import qualified Day2

runDay :: Int -> IO ()
runDay 1 = Day1.run
runDay 2 = Day2.run

main :: IO ()
main = do
  runDay 2
