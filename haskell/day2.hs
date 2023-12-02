module Day2 where
import Util
import qualified Aoc

type RGB = (Int, Int, Int)
type Game = (Int, [RGB])

parse :: [String] -> [Game]
parse = map parseGame

parseGame :: String -> Game
parseGame s =
  let (lhs, rhs) = splitPair ':' s in
  let id = read (words lhs !! 1) in
  let draws = map parseRGB $ splitList ';' rhs in
  (id, draws)

parseRGB :: String -> RGB
parseRGB = foldl1 addRGB . map parseComponent . splitList ','

parseComponent :: String -> RGB
parseComponent s = case words s of
  [n, "red"] -> (read n, 0, 0)
  [n, "green"] -> (0, read n, 0)
  [n, "blue"] -> (0, 0, read n)
  _ -> error ("Invalid colour component: " ++ s)

addRGB :: RGB -> RGB -> RGB
addRGB (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

maxRGB :: RGB -> RGB -> RGB
maxRGB (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

power :: RGB -> Int
power (r, g, b) = r * g * b
  
part1 games =
  sum [id | (id, draws) <- games, all check draws]
  where check (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 games = sum $ map (power . foldl1 maxRGB . snd) games

run = Aoc.run 2 parse part1 part2
