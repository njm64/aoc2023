-- This is a simple Leftist heap based on the implementation here:
-- https://en.wikipedia.org/wiki/Leftist_tree

module Heap where

data Heap a b = Leaf | Node a b (Heap a b) (Heap a b)

empty :: Heap a b
empty = Leaf
  
-- TODO: Should we store the rank in the node?  
rank :: Heap a b -> Int
rank Leaf = 0
rank (Node _ _ _ r) = rank r + 1

merge :: Ord a => Heap a b -> Heap a b -> Heap a b
merge Leaf h = h
merge h Leaf = h
merge h@(Node a b l r) h'@(Node a' _ _ _)
  | a > a' = merge h' h
  | rank r' > rank l = Node a b r' l
  | otherwise = Node a b l r'
  where r' = merge r h'

push :: Ord a => Heap a b -> a -> b -> Heap a b
push h a b = merge h (Node a b Leaf Leaf)
  
pop :: Ord a => Heap a b -> Heap a b
pop (Node _ _ l r) = merge l r
  
top :: Heap a b -> Maybe (a, b)
top Leaf = Nothing
top (Node a b _ _) = Just (a, b)

