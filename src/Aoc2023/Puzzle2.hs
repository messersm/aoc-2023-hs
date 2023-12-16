module Aoc2023.Puzzle2 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (foldl')
import Text.ParserCombinators.ReadP

data Color
  = Red Integer
  | Green Integer
  | Blue Integer
  deriving (Show, Read)

data Game = Game Integer [[Color]] deriving (Show, Read)

integer :: ReadP Integer
integer = read <$> (many1 $ satisfy isDigit)

color :: ReadP Color
color = do
  n <- integer
  skipSpaces
  c <- string "green" <|> string "red" <|> string "blue"

  case c of
    "green" -> return $ Green n
    "red"   -> return $ Red n
    "blue"  -> return $ Blue n
    _       -> pfail

colors :: ReadP [Color]
colors = sepBy1 color (string ", ")

game :: ReadP Game
game = do
  _ <- string "Game "
  gameId <- integer
  _ <- string ": "
  drawings <- sepBy1 colors (string "; ")
  return $ Game gameId drawings

games :: ReadP [Game]
games = do
  gs <- sepBy game (string "\n")
  skipSpaces
  optional $ string "\n"
  eof
  return gs

-- | Count the colors of a list of colors
--
-- Example:
--
-- >>> counts [Red 1, Green 3, Blue 6, Red 6]
-- (7,3,6)
counts :: [Color] -> (Integer, Integer, Integer)
counts = foldl' f (0, 0, 0)
  where
    f (r, g, b) (Red   n) = (r + n, g, b)
    f (r, g, b) (Green n) = (r, g + n, b)
    f (r, g, b) (Blue  n) = (r, g, b + n)

-- | Check, if the drawing of the given colors is possible if
-- | (r, g, b) exist in total.
--
-- Example:
--
-- >>> possible (1, 2, 3) [Red 2, Green 0, Blue 0]
-- False
-- >>> possible (1, 2, 3) [Red 1, Green 2, Blue 3]
-- True
--
-- The following properties hold:
--
-- prop> possible (r, g, b) [Red r, Green g, Blue b] == True
-- prop> possible (r, g, b) [Red (r-1), Green g, Blue b] == True
-- prop> possible (r, g, b) [Red (r+1), Green g, Blue b] == False
possible :: (Integer, Integer, Integer) -> [Color] -> Bool
possible (r, g, b) colors
    | r' > r    = False
    | g' > g    = False
    | b' > b    = False
    | otherwise = True
  where
    (r', g', b') = counts colors

fewest :: [[Color]] -> (Integer, Integer, Integer)
fewest = foldl' f (0, 0, 0)
  where
    f (r, g, b) cs
      = let (r', g', b') = counts cs
        in (max r r', max g g', max b b')

power :: (Integer, Integer, Integer) -> Integer
power (r, g, b) = r * g * b

part1 :: String -> Integer
part1 content = sum [gameId | Game gameId ccs <- gs, and $ possible (12, 13, 14) <$> ccs]
  where
    gs = fst $ head $ readP_to_S games content

part2 :: String -> Integer
part2 content = sum [power $ fewest ccs | Game _ ccs <- gs]
  where
    gs = fst $ head $ readP_to_S games content
