module Day20 where
import qualified Aoc
import qualified Data.Map as Map
import Data.Maybe ( fromJust, maybeToList )
import Data.List
import Util

data PulseType = High | Low deriving (Show, Eq)

data Pulse = Pulse {
  pulseType :: PulseType,
  pulseSrc :: String,
  pulseDst :: String
} deriving (Show)

data Module = Broadcaster |
              FlipFlop Bool |
              Conjunction (Map.Map String PulseType)
              deriving (Show)

type System = Map.Map String Module
type ConnectionMap = Map.Map String [String]

applyPulse :: Module -> PulseType -> String -> (Module, Maybe PulseType)

-- Applying a high pulse to a FlipFlop does nothing. Applying a low pulse
-- to a FlipFlip toggles the state and sends out a pulse.
applyPulse (FlipFlop state) High _ = (FlipFlop state, Nothing)
applyPulse (FlipFlop False) Low _ = (FlipFlop True, Just High)
applyPulse (FlipFlop True) Low _ = (FlipFlop False, Just Low)

-- Applying a pulse to a Conjunction saves the pulse associated with this
-- source, and then sends out a low pulse if all inputs are high, or a
-- high pulse otherwise.
applyPulse (Conjunction m) pulse source = (Conjunction m', Just output)
  where m' = Map.insert source pulse m
        output = if all (== High) (Map.elems m') then Low else High
applyPulse Broadcaster pulse _ = (Broadcaster, Just pulse)
  
-- Apply a pulse to a system, returning an updated system and a list of
-- output pulses.
applyPulseToSystem :: System -> ConnectionMap -> Pulse -> (System, [Pulse])
applyPulseToSystem sys cmap p =
  let id = pulseDst p in
  case Map.lookup id sys of
    Nothing -> (sys, [])
    Just mod ->
      let (mod', outputType) = applyPulse mod (pulseType p) (pulseSrc p) in
      let sys' = Map.insert id mod' sys in
        (sys', [Pulse {pulseType=t, pulseSrc=id, pulseDst=d} |
                t <- maybeToList outputType,
                d <- Map.findWithDefault [] id cmap])
        
-- Apply a list of pulses to a system, returning an updated system and a list
-- of output pulses.
applyPulsesToSystem :: System -> ConnectionMap -> [Pulse] -> (System, [Pulse])
applyPulsesToSystem s cmap [] = (s, [])
applyPulsesToSystem s cmap (p:ps) =
  let (s', output) = applyPulseToSystem s cmap p in
  let (s'', output') = applyPulsesToSystem s' cmap ps in
  (s'', output ++ output')
  
buttonPulse = Pulse {
  pulseType=Low,
  pulseSrc="button",
  pulseDst="broadcaster"
}
  
pushButton :: System -> ConnectionMap -> (System, [Pulse])
pushButton s cm =
  go s [buttonPulse] [[buttonPulse]]
  where go s [] pulseGroups = (s, concat $ reverse pulseGroups)
        go s pulses pulseGroups =
          let (s', pulses') = applyPulsesToSystem s cm pulses in
          go s' pulses' (pulses':pulseGroups)
            
parseModule :: String -> (String, Module, [String])
parseModule s =
  let tokens = words $ replace "," "" s in
  let connections = drop 2 tokens in
  case head tokens of
    ('%':id) -> (id, FlipFlop False, connections)
    ('&':id) -> (id, Conjunction Map.empty, connections)
    "broadcaster" -> ("broadcaster", Broadcaster, connections)
    _ -> error "Unknown module type"
           
-- Convert an output connection map to an input connection map
invertMap :: ConnectionMap -> ConnectionMap
invertMap m = 
  Map.fromListWith (++) [(v, [k]) | (k, vs) <- Map.assocs m, v <- vs]
  
-- Populate the input maps for all conjunctions in the system
addConjunctionInputs :: System -> ConnectionMap -> System
addConjunctionInputs s inputMap = Map.mapWithKey f s
  where f id (Conjunction _) =
          let inputs = Map.findWithDefault [] id inputMap in
            Conjunction (Map.fromList (zip inputs (repeat Low)))
        f _ m = m
  
parse :: [String] -> (System, ConnectionMap)
parse lines =
  let ms = map parseModule lines in
  let system = Map.fromList [(id, m) | (id, m, _ ) <- ms] in
  let connectionMap = Map.fromList [(id, cs) | (id, _, cs) <- ms] in
  let system' = addConjunctionInputs system (invertMap connectionMap) in
  (system', connectionMap)  
  
-- Count the number of button presses until the module with the given
-- ID receives a low pulse.
countPressesForID :: System -> ConnectionMap -> String -> Int
countPressesForID sys cmap id =
  go sys 1
  where go s n =
          let (s', pulses) = pushButton s cmap in
          case find (\p -> pulseType p == Low && pulseDst p == id) pulses of
            Just _ -> n
            Nothing -> go s' (n + 1)
  
part1 (sys, cmap) =
  go sys 1000 0 0
  where go s 0 loCount hiCount = loCount * hiCount
        go s n loCount hiCount =
          let (s', pulses) = pushButton s cmap in
          let lo = length $ filter (\p -> pulseType p == Low) pulses in
          let hi = length pulses - lo in
          go s' (n - 1) (loCount + lo) (hiCount + hi)
  
part2 (sys, cmap) =
  -- The objective is to count the number of button pushes to activate rx.
  -- In my input at least, the input for rx is a conjunction nr, which is in
  -- turn connected to 4 conjunctions (lh, fk, ff, and mm). These only receive
  -- a low pulse every ~4000 cycles. We need to find how many button presses
  -- until they are all low in the same cycle, and this turns out to be the
  -- product of their individual counts.
  product $ map (countPressesForID sys cmap) ["lh", "fk", "ff", "mm"]
    
run = Aoc.run 20 parse part1 part2
