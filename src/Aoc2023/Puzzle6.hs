{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aoc2023.Puzzle6 where

import Data.Char
import Text.ParserCombinators.ReadP

import Lib.ReadP

newtype Time = Time Integer deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype Distance = Distance Integer deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

parser :: ReadP [(Time, Distance)]
parser = do
  string "Time:" *> skipSpaces
  times <- natural `sepBy` (space *> skipSpaces)
  newline
  string "Distance:" *> skipSpaces
  distances <- natural `sepBy` (space *> skipSpaces)
  skipSpaces
  eof

  return $ zip (Time <$> times) (Distance <$> distances)

parser2 :: ReadP [(Time, Distance)]
parser2 = do
  string "Time:" *> skipSpaces
  time <- (read . concat) <$> (many1 $ satisfy isDigit) `sepBy` (space *> skipSpaces)
  newline
  string "Distance:" *> skipSpaces
  distance <- (read . concat) <$> (many1 $ satisfy isDigit) `sepBy` (space *> skipSpaces)
  skipSpaces
  eof

  return [(Time time, Distance distance)]

-- | Check if a real number is an integer.
--
-- Examples:
--
-- >>> isInteger 1.32423
-- False
-- >>> isInteger 1.0
-- True
isInteger :: RealFrac a => a -> Bool
isInteger x = fromInteger (round x) == x

-- | Given the total time `t` and the record distance `r` of the race,
-- | we need to solve the inequality `h (t * h) > r`
-- | with `h` being the hold time.
solve :: Time -> Distance -> (Time, Time)
solve t r = (Time t1, Time t2)
  where
    p :: Double = fromIntegral t
    q :: Double = fromIntegral r

    delta = sqrt (p ^ 2 / 4 - q)

    t_min = p / 2 - delta
    t_max = p / 2 + delta

    t1 = if isInteger t_min then round $ t_min + 1 else ceiling t_min
    t2 = if isInteger t_max then round $ t_max - 1 else floor t_max

countSolutions :: (Time, Time) -> Integer
countSolutions (t1, t2) = fromIntegral $ t2 - t1 + (Time 1)

-- |
-- 
--     d(h, t) > d_record
-- <=> h * (t - h) > d_record
-- <=> h * t - h ^ 2 - d_record > 0
-- <=> h ^ 2 - h * t + d_record < 0
-- <=> (h - t / 2) ^ 2 - t ^ 2 / 4 + d_record < 0
part1 s = product $ countSolutions . (uncurry solve) <$> (fst $ head $ readP_to_S parser s)
part2 s = product $ countSolutions . (uncurry solve) <$> (fst $ head $ readP_to_S parser2 s)
