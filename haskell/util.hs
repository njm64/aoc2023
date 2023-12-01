module Util where

import qualified Data.List as List
import qualified Data.Text as Text

(|>) x f = f x

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

