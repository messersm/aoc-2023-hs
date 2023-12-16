module Aoc2023.Puzzle4 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (foldl')
import Text.ParserCombinators.ReadP

import Data.Set (Set, intersection, size, fromList)

data Card = Card Int (Set Int) (Set Int) deriving (Read, Show)

-- | ReadP parser for non-negative integers
--
-- Examples:
--
-- >>> readP_to_S (natural <* eof) "411"
-- [(411,"")]
-- >>> readP_to_S (natural <* eof) "0"
-- [(0,"")]
-- >>> readP_to_S (natural <* eof) "sometext0"
-- []
-- >>> readP_to_S (natural <* eof) "-20"
-- []
natural :: ReadP Int
natural = read <$> (many1 $ satisfy isDigit)

newline :: ReadP Char
newline = char '\n'

-- | ReadP parser for Card
--
-- Example:
--
-- >>> fst $ head $ readP_to_S (card <* eof) "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- Card 1 (fromList [17,41,48,83,86]) (fromList [6,9,17,31,48,53,83,86])
card :: ReadP Card
card = do
  _ <- string "Card"
  skipSpaces
  cardId <- natural
  _ <- string ":"
  skipSpaces
  winning <- natural `sepBy` many1 (char ' ')
  skipSpaces
  _ <- string "|"
  skipSpaces
  drawn <- natural `sepBy` many1 (char ' ')
  return $ Card cardId (fromList winning) (fromList drawn)

score :: Card -> Int
score (Card _ winning drawn)
  = case size $ winning `intersection` drawn of
    0 -> 0
    n -> 2 ^ (n - 1)

-- part1 :: String -> Integer
part1 content = sum $ score <$> cards
  where
    parser = (card `sepBy` newline <* optional newline <* eof)
    cards = fst $ head $ readP_to_S parser content

part2 :: String -> Integer
part2 content = undefined
