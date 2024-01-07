module Aoc2023.Puzzle7 where

import Control.Applicative hiding (many)
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Text.ParserCombinators.ReadP

import qualified Data.Map.Strict as Map

import Lib.ReadP

data Card
  = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving (Show, Eq, Ord)

newtype WithJoker = WithJoker Card deriving (Show, Eq)

instance Ord WithJoker where
  compare (WithJoker J) (WithJoker J) = EQ
  compare (WithJoker J) (WithJoker _) = LT
  compare (WithJoker _) (WithJoker J) = GT
  compare (WithJoker x) (WithJoker y) = compare x y

type Hand = [Card]
type HandWithJoker = [WithJoker]
type Bid  = Int

data HandType
  = HighCard  | OnePair     | TwoPair | ThreeOfAKind
  | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord)

cardP :: ReadP Card
cardP
   =  char '2' *> return Two
  <|> char '3' *> return Three
  <|> char '4' *> return Four
  <|> char '5' *> return Five
  <|> char '6' *> return Six
  <|> char '7' *> return Seven
  <|> char '8' *> return Eight
  <|> char '9' *> return Nine
  <|> char 'T' *> return T
  <|> char 'J' *> return J
  <|> char 'Q' *> return Q
  <|> char 'K' *> return K
  <|> char 'A' *> return A

parser :: ReadP [(Hand, Bid)]
parser = many (
  do
    hand <- sequence [cardP, cardP, cardP, cardP, cardP]
    skipSpaces
    bid <- natural
    skipSpaces
    return (hand, bid)
  ) <* eof

counts :: (Ord a) => [a] -> Map a Int
counts = f Map.empty
  where
    f m [] = m
    f m (x:xs) = f m' xs
      where
        m' = Map.alter (\c -> (+1) <$> c <|> Just 1) x m

handtypeFromSignature :: [Int] -> HandType
handtypeFromSignature xs = case xs of
  [1, 1, 1, 1, 1] -> HighCard
  [1, 1, 1, 2]    -> OnePair
  [1, 1, 3]       -> ThreeOfAKind
  [1, 2, 2]       -> TwoPair
  [1, 4]          -> FourOfAKind
  [2, 3]          -> FullHouse
  _               -> FiveOfAKind

-- | Compute the type of a hand.
--
-- Examples:
--
-- >>> handtype [Three, Two, T, Three, K]
-- OnePair
-- >>> handtype [T, Five, Five, J, Five]
-- ThreeOfAKind
-- >>> handtype [K, K, Six, Seven, Seven]
-- TwoPair
handtype :: Hand -> HandType
handtype h = handtypeFromSignature $ sort $ Map.elems (counts h)

handtypeWithJoker :: HandWithJoker -> HandType
handtypeWithJoker h = handtypeFromSignature sig'
  where
    m = counts h
    jokerCount = fromJust $ Map.lookup (WithJoker J) m <|> Just 0
    m' = Map.delete (WithJoker J) m

    sig = sort $ Map.elems m'
    sig' = if null sig then [5] else init sig <> [last sig + jokerCount]

part1 s = output
  where
    input = fst $ head $ readP_to_S parser s
    sorted = sort $ [(handtype hand, hand, bid) | (hand, bid) <- input]
    output = sum [rank * bid | (rank, bid) <- zip [1..] [b | (_, _, b) <- sorted]]
  
part2 s = output
  where
    input = fst $ head $ readP_to_S parser s
    mapped = [(WithJoker <$> h, b) | (h, b) <- input]
    sorted = sort $ [(handtypeWithJoker hand, hand, bid) | (hand, bid) <- mapped]
    output = sum [rank * bid | (rank, bid) <- zip [1..] [b | (_, _, b) <- sorted]]
