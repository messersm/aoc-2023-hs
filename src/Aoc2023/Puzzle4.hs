module Aoc2023.Puzzle4 where

import Lib.ReadP

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, get, put, runState, execState)
import Control.Monad
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (foldl')
import Text.ParserCombinators.ReadP hiding (get)

import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Card = Card Int (Set Int) (Set Int) deriving (Read, Show)

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
  return $ Card cardId (Set.fromList winning) (Set.fromList drawn)

matching :: Card -> Int
matching (Card _ winning drawn) = Set.size $ winning `Set.intersection` drawn

score :: Card -> Int
score c
    | n == 0    = 0
    | otherwise = 2 ^ (n - 1)
  where
    n = matching c

toMap :: [Card] -> Map Int (Card, Int)
toMap cards = Map.fromList [(cardId, (c, 1)) | c@(Card cardId _ _) <- cards]

grow :: State (Map Int (Card, Int)) ()
grow = do
  ks <- Map.keys <$> get

  forM_ ks $ \k -> do
    m <- get
    let x :: Map Int (Card, Int)
        x = m
    let (card, count) = (m ! k)
        n = matching card
        ids = [k+1..k+n]

    forM_ ids $ \k' -> do
      m' <- get
      put $ Map.adjust (\(card', count') -> (card', count' + count)) k' m'

-- part1 :: String -> Integer
part1 content = sum $ score <$> cards
  where
    parser = (card `sepBy` newline <* optional newline <* eof)
    cards = fst $ head $ readP_to_S parser content

part2 :: String -> Int
part2 content = sum [n | (Card _ _ _, n) <- Map.elems $ execState grow $ toMap cards ]
    where
      parser = (card `sepBy` newline <* optional newline <* eof)
      cards = fst $ head $ readP_to_S parser content
