module Day19 where
import qualified Aoc
import Util

data Operator = OpLT | OpGT deriving (Eq, Show)
type Category = Int
type Rule = (Category, Operator, Int, String)
type Part = [Int]
type Workflow = (String, ([Rule], String))

data WorkTree = Accept |
                Reject |
                Compare Category Int WorkTree WorkTree
                deriving (Show)
  
parseRule :: String -> Rule
parseRule s =
  let (lhs, rhs) = splitPair ':' s
      cat = case head s of 
          'x' -> 0
          'm' -> 1
          'a' -> 2
          's' -> 3
      op = case s !! 1 of
          '>' -> OpGT
          '<' -> OpLT
      val = read $ drop 2 lhs
      dst = rhs 
  in (cat, op, val, dst)
    
parseWorkflow :: String -> Workflow
parseWorkflow s =
  let (id, rhs) = splitPair '{' (init s) in
  let rules = splitList ',' rhs in
  (id, (map parseRule $ init rules, last rules))
  
parsePart :: String -> Part
parsePart = map (read . drop 2) . splitList ',' . init . tail 
  
parse :: [String] -> ([Workflow], [Part])
parse lines = let (ws, ps) = splitPair "" lines in
  (map parseWorkflow ws, map parsePart ps)
  
-- Convert the list of workflows into a binary tree, with leaf
-- nodes for Accept and Reject, and a branch node for
-- each comparison.
buildTree :: [Workflow] -> WorkTree
buildTree ws = treeForWorkflow "in"
  where treeForWorkflow :: String -> WorkTree
        treeForWorkflow "A" = Accept
        treeForWorkflow "R" = Reject
        treeForWorkflow id =
          case lookup id ws of
            Just (rules, dst) -> treeForRules rules dst
            Nothing -> error "Invalid workflow name"
        treeForRules :: [Rule] -> String -> WorkTree
        treeForRules [] defaultDst = treeForWorkflow defaultDst
        treeForRules ((cat, op, val, dst):rs) defaultDst =
          let left = treeForWorkflow dst in  
          let right = treeForRules rs defaultDst in
            case op of
              OpLT -> Compare cat val left right
              OpGT -> Compare cat (val + 1) right left

checkPart :: WorkTree -> Part -> Bool
checkPart Accept _ = True
checkPart Reject _ = False
checkPart (Compare cat val left right) p
  | p !! cat < val = checkPart left p
  | otherwise = checkPart right p
  
-- To count the total number of possibilities, we start with
-- ranges of (1,4000) for XMAS, and then narrow these ranges
-- for each branch of the tree. If we reach an Accept node,
-- take the product of the 4 ranges, then add all the Accept
-- nodes.
countPaths :: WorkTree -> Int
countPaths = go $ replicate 4 (1, 4000)
  where go b Reject = 0
        go b Accept = product $ map (\(r1, r2) -> r2 - r1 + 1) b
        go b (Compare cat val left right) = 
          go (updateNth (updateMax (val - 1)) cat b) left +
          go (updateNth (updateMin val) cat b) right
        updateMin v (rMin, rMax) = (max rMin v, rMax)
        updateMax v (rMin, rMax) = (rMin, min rMax v)
  
part1 (workflows, parts) =
  let tree = buildTree workflows in
  sum . concat $ filter (checkPart tree) parts
  
part2 (workflows, parts) = countPaths $ buildTree workflows
run = Aoc.run 19 parse part1 part2

