{-# LANGUAGE FlexibleContexts #-}

module Aoc2023.Puzzle3 where

import Control.Monad
import Data.Char
import Data.Functor.Identity
import Text.Parsec

import Data.Set (Set)
import qualified Data.Set as Set

data Schema
  = PartNumber Int (Int, Int) (Int, Int)
  | Symbol     Char (Int, Int)
  deriving (Show, Read)


partNumber :: Stream s Identity Char => Parsec s u Schema
partNumber = do
  start <- getPosition
  n <- read <$> many1 digit
  end <- getPosition
  return $ PartNumber n (sourceLine start, sourceColumn start) (sourceLine end, sourceColumn end)

symbol :: Stream s Identity Char => Parsec s u Schema
symbol = do
  start <- getPosition
  c <- satisfy (\c -> c /= '.' && not (isAlphaNum c) && not (isControl c))
  return $ Symbol c (sourceLine start, sourceColumn start)

schema :: Stream s Identity Char => Parsec s u [Schema]
schema = do
  s <- concat <$> many
     (  pure <$> partNumber
    <|> pure <$> symbol
    <|> (char '.' >> return [])
    <|> (endOfLine >> return []))
  eof
  return s


symbolPositions :: [Schema] -> Set (Int, Int)
symbolPositions s = Set.fromList [p | Symbol _ p <- s]

-- | Return all positions 
surrounding :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
surrounding (l1, c1) (l2, c2)
  = Set.fromList [(l, c) | l <- [l1-1 .. l2+1], c <- [c1 - 1 .. c2]]

isPart :: Set (Int, Int) -> Schema -> Bool
isPart _ (Symbol _ _) = False
isPart p (PartNumber _ start end)
  = not $ null $ surrounding start end `Set.intersection` p

part1 :: String -> Int
part1 content
  = case parse schema "puzzle 3" content of
    Right items ->
      let p = symbolPositions items
      in sum [ x | n@(PartNumber x _ _) <- items, isPart p n ]
    Left _ -> error "Invalid input"


gearRatios :: [Schema] -> [Int]
gearRatios items = do
  pos <- [Set.singleton p | g@(Symbol '*' p) <- items]
  let numbers = [ x | n@(PartNumber x _ _) <- items, isPart pos n ]
  guard (length numbers == 2)
  return $ product numbers

-- part2 :: String -> Int
part2 content
  = case parse schema "puzzle 3" content of
    Right items -> sum $ gearRatios items
    Left _ -> error "Invalid input"
