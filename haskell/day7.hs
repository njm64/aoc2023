module Day7 where
import qualified Aoc
import Data.Char
import Data.List

type Bid = Int
type Card = Char
type Hand = ([Card], Bid)
  
data Label = HighCard | OnePair | TwoPair | ThreeOfAKind |
             FullHouse | FourOfAKind | FiveOfAKind
             deriving (Eq, Ord, Show)
  
parseHand :: String -> Hand
parseHand s =
  let [cards, bid] = words s in
  (cards, read bid)

parse :: [String] -> [Hand]
parse = map parseHand

cardValue :: Card -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = 11
cardValue 'T' = 10
cardValue c
  | isDigit c = ord c - ord '0'
  | otherwise = error "invalid card"
  
cardValue2 :: Card -> Int
cardValue2 'J' = 1
cardValue2 c = cardValue c

handType :: [Card] -> Label
handType cs = case sort . map length . group . sort $ cs of
  [5] -> FiveOfAKind
  [1,4] -> FourOfAKind
  [2,3] -> FullHouse
  [1,1,3] -> ThreeOfAKind
  [1,2,2] -> TwoPair
  [1,1,1,2] -> OnePair
  [1,1,1,1,1] -> HighCard
  _ -> error "Invalid hand"

handType2 :: [Card] -> Label
handType2 cs = case sort . map length . group . sort $ filter (/= 'J') cs of
  -- 1 Joker
  [4] -> FiveOfAKind
  [1,3] -> FourOfAKind
  [2,2] -> FullHouse
  [1,1,2] -> ThreeOfAKind
  [1,1,1,1] -> OnePair
  -- 2 Jokers
  [3] -> FiveOfAKind
  [1,2] -> FourOfAKind
  [1,1,1] -> ThreeOfAKind
  -- 3 Jokers
  [2] -> FiveOfAKind
  [1,1] -> FourOfAKind
  -- 4 Jokers
  [1] -> FiveOfAKind
  _ -> handType cs
  
calc :: (Ord a) => ([Card] -> a) -> [Hand] -> Int
calc f hands =
  let sortedBids = map snd $ sortOn (f . fst) hands in
  sum [bid*rank | (bid,rank) <- zip sortedBids [1..]]
  
part1 = calc (\cs -> (handType cs, map cardValue cs))
part2 = calc (\cs -> (handType2 cs, map cardValue2 cs))
run = Aoc.run 7 parse part1 part2

