module Util where

import qualified Data.List as List
import qualified Data.Text as Text
import Data.Array as Array

-- Split a list exactly once at the given delimiter
splitPair :: Eq a => a -> [a] -> ([a], [a])
splitPair c s = case break (== c) s of
  (x, []) -> (x, [])
  (x, xs) -> (x, tail xs)

-- Split a list on elements that match the given predicate
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = case break f xs of
  (ys, []) -> [ys]
  (ys, z:[]) -> ys : [[]]
  (ys, _:zs) -> ys : splitWith f zs

-- Split a list on elements that are equal to c
splitList :: Eq a => a -> [a] -> [[a]]
splitList c = splitWith (== c)

-- Split a list into chunks of length n
chunks :: Int -> [a] -> [[a]]
chunks n lst = case splitAt n lst of
  (chunk, []) -> [chunk]
  (chunk, rest) -> chunk : (chunks n rest)

replace :: String -> String -> String -> String
replace from to = 
  let from' = Text.pack from
      to' = Text.pack to
  in Text.unpack . (Text.replace from' to') . Text.pack

listToPair :: [a] -> (a, a)
listToPair [a,b] = (a, b)

 -- Find the first index of a substring
position :: String -> String -> Int
position s substr =
  step s 0
  where step [] acc = -1
        step t acc
          | substr `List.isPrefixOf` t = acc
          | otherwise = step (tail t) (acc + 1)

-- Like Data.List.groupBy, but compare with the nearest
-- neighbour, instead of the last element in the group.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f [] = []
groupBy' f (x:xs) = step xs [[x]]
  where step [] acc = reverse . map reverse $ acc
        step (x:xs) (a:as) =
          if f (head a) x then step xs ((x:a):as)
          else step xs ([x]:a:as)

groupOn :: Eq a => (t -> a) -> [t] -> [[t]]
groupOn f = groupBy' (\a b -> f a == f b)
  
arrayRows :: Array.Array (Int, Int) a -> [[a]]
arrayRows a =
  [row y | y <- [y1..y2]]
  where row y = [a ! (x, y) | x <- [x1..x2]]
        ((x1, y1), (x2, y2)) = bounds a

showCharArray :: Array.Array (Int, Int) Char -> String
showCharArray = unlines . arrayRows
