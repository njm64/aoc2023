module Day17 where
import qualified Aoc
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Heap

data Direction = North | South | East | West deriving (Show, Ord, Eq)
type MoveCount = Int
type Cost = Int
type State = (Pos, Direction, MoveCount)
type GoalFn = (State -> Bool)
type NeighboursFn = State -> [State]
type Pos = (Int, Int)
type CostMap = Map.Map State Cost
type HeatMap = Array Pos Int

parse :: [String] -> HeatMap 
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  let cells = map digitToInt (concat lines) in
  array ((1,1), (width, height)) (zip coords cells)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

move :: Pos -> Direction -> Pos
move (x,y) North = (x, y-1)
move (x,y) South = (x, y+1)
move (x,y) East = (x+1, y)
move (x,y) West = (x-1, y)

moveState :: State -> Direction -> State
moveState (pos, dir, moveCount) newDir
  | dir == newDir = (move pos newDir, newDir, moveCount + 1)
  | otherwise = (move pos newDir, newDir, 1)
  
search :: HeatMap -> State -> NeighboursFn -> GoalFn -> Int
search m initialState getNeighbours isGoal =
  go (Heap.push Heap.empty 0 initialState) Map.empty where
  go heap costMap =
    case Heap.top heap of
      Nothing -> error "Not found"
      Just (cost, state) ->
        if isGoal state then cost
        else
          let items = [(c, s) | s@(pos, _, _) <- getNeighbours state,
                                inRange (bounds m) pos,
                                let c = cost + (m ! pos),
                                checkCost costMap c s]
              heap' = foldl' updateHeap (Heap.pop heap) items
              costMap' = foldl' updateMap costMap ((-1, state):items)
          in
          go heap' costMap'
  updateHeap h (cost, state) = Heap.push h cost state
  updateMap m (cost, state) = Map.insert state cost m
  checkCost m cost state = case Map.lookup state m of
    Just existing -> cost < existing
    Nothing -> True
    
part1 m =
  search m state neighbours isGoal
  where state = ((1, 1), East, 0) 
        goal = snd (bounds m)
        isGoal (p, _, moves) = p == goal
        neighbours s@(_, dir, moveCount) =
          catMaybes [straight, left, right]
          where straight 
                  | moveCount < 3 = Just (moveState s dir)
                  | otherwise = Nothing
                left = Just (moveState s (turnLeft dir))
                right = Just (moveState s (turnRight dir))
  
part2 m =
  search m state neighbours isGoal
  where state = ((1, 1), East, 0) 
        goal = snd (bounds m)
        isGoal (p, _, moves) = p == goal && moves >= 4
        neighbours s@(_, dir, moveCount) =
          catMaybes [straight, left, right]
          where straight 
                  | moveCount < 10 = Just (moveState s dir)
                  | otherwise = Nothing
                left 
                  | moveCount >= 4 = Just (moveState s (turnLeft dir))
                  | otherwise = Nothing
                right
                  | moveCount >= 4 = Just (moveState s (turnRight dir))
                  | otherwise = Nothing

run = Aoc.run 17 parse part1 part2
