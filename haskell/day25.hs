module Day25 where
import qualified Aoc
import Util
import Data.List
import Data.Maybe

type Edge = (String, String)
  
parseLine :: String -> [Edge]
parseLine s =
  let (a, rhs) = splitPair ':' s in
  [(a, b) | b <- words rhs]

parse :: [String] -> [Edge]
parse = concatMap parseLine

vertexCount :: [Edge] -> Int
vertexCount es = length . nub $ (map fst es ++ map snd es)
  
contractEdge :: [Edge] -> Edge -> String -> [Edge]
contractEdge es (u, v) newEdgeName =
  map rename . filter (/= (u, v)) . filter (/= (v, u)) $ es
  where rename (a, b)
          | a == u || a == v = (newEdgeName, b)
          | b == u || b == v = (a, newEdgeName)
          | otherwise = (a, b)

vertexWeight :: String -> Int
vertexWeight s =
  case elemIndex '.' s of
    Nothing -> 1
    Just i -> read $ drop (i + 1) s
  
-- New vertices are named "a.b", where a is a counter to
-- ensure uniqueness, and b is the total number of vertices that
-- have been contracted into this one.
makeVertexName :: Edge -> Int -> String
makeVertexName (a, b) i =
  show i ++ "." ++ show (vertexWeight a + vertexWeight b)
  
-- Not very random, but seems to work better than using a real RNG,
-- at least for my input.
nextRandom :: Int -> Int -> Int
nextRandom r seed = r + seed
  
-- Run Karger's algorithm on a set of edges, contracting them until
-- there are only 2 vertices left.
karger :: [Edge] -> Int -> [Edge]
karger es seed =
  go (vertexCount es) es 0 seed
  where
    go 2 es _ _ = es
    go v es r i =
      let e = es !! (r `mod` length es) in
      let es' = contractEdge es e (makeVertexName e i)  in
      go (v - 1) es' (nextRandom r seed) (i + 1)
  
calcScore :: [Edge] -> Int
calcScore ((a, b):_) = vertexWeight a * vertexWeight b
  
-- Keep running Karger's algorithm with different random seeds until
-- we find a minimum cut of size 3, then calculate the score.
part1 es =
  head [calcScore es' | seed <- [1..], let es' = karger es seed, length es' == 3]

run = Aoc.run 25 parse part1 (const 0)

