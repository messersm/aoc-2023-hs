{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Aoc2023.Puzzle5 where

import Lib.ReadP

import Control.Applicative hiding (many, optional)
import Data.Bifunctor
import Data.Functor
import Data.Map.Lazy (Map)
import Data.Maybe
import GHC.Data.Maybe
import Text.ParserCombinators.ReadP

import qualified Data.Map.Lazy as Map

newtype Seed = Seed Int deriving (Show, Eq, Ord, Enum)
newtype Soil = Soil Int deriving (Show, Eq, Ord, Enum)
newtype Fertilizer = Fertilizer Int deriving (Show, Eq, Ord, Enum)
newtype Water = Water Int deriving (Show, Eq, Ord, Enum)
newtype Light = Light Int deriving (Show, Eq, Ord, Enum)
newtype Temperature = Temperature Int deriving (Read, Show, Eq, Ord, Enum)
newtype Humidity = Humidity Int deriving (Show, Eq, Ord, Enum)
newtype Location = Location Int deriving (Show, Eq, Ord, Enum)

newtype RangeMap a b = RangeMap [(a, b, Int)]


reverse :: RangeMap a b -> RangeMap b a
reverse (RangeMap rs) = RangeMap [(y, x, r) | (x, y, r) <- rs]

instance (Show a, Show b) => Show (RangeMap a b) where
  show (RangeMap rs) = "RangeMap " ++ show rs

toFunction :: (Enum a, Enum b) => (a, b, Int) -> a -> Maybe b
toFunction (x, y, r) x'
    | value <  start = Nothing
    | value >= end   = Nothing
    | otherwise      = Just $ toEnum $ fromEnum y - start + value
  where
    value = fromEnum x'
    start = fromEnum x
    end = start + r

(!?) :: (Enum a, Enum b) => RangeMap a b -> a -> Maybe b
(RangeMap rs) !? k = firstJusts $ toFunction <$> rs <*> [k]

infixr 6 !?!
(!?!) :: (Enum a, Enum b) => RangeMap a b -> a -> b
m !?! k = m !? k `orElse` (toEnum $ fromEnum k)

data Almanac = Almanac
  { seeds :: [Seed]
  , seedToSoil :: RangeMap Seed Soil
  , soilToFertilizer :: RangeMap Soil Fertilizer
  , fertilizerToWater :: RangeMap Fertilizer Water
  , waterToLight :: RangeMap Water Light
  , lightToTemperature :: RangeMap Light Temperature
  , temperatureToHumidity :: RangeMap Temperature Humidity
  , humidityToLocation :: RangeMap Humidity Location
  }
  deriving (Show)

mapping :: (Enum a, Enum b) => ReadP (RangeMap a b)
mapping = do
  xs <- many $ do
    dest <- natural
    string " " *> skipSpaces
    src <- natural
    string " " *> skipSpaces
    range <- natural
    optional newline

    return (toEnum src, toEnum dest, range)
  
  skipSpaces
  return $ RangeMap xs

almanacP :: ReadP Almanac
almanacP = do
  seeds <- string "seeds: " *> ((fmap Seed) <$> natural `sepBy` space)
  skipSpaces

  string "seed-to-soil map:" *> skipSpaces
  seedToSoil <- mapping

  string "soil-to-fertilizer map:" *> skipSpaces
  soilToFertilizer <- mapping

  string "fertilizer-to-water map:" *> skipSpaces
  fertilizerToWater <- mapping

  string "water-to-light map:" *> skipSpaces
  waterToLight <- mapping

  string "light-to-temperature map:" *> skipSpaces
  lightToTemperature <- mapping

  string "temperature-to-humidity map:" *> skipSpaces
  temperatureToHumidity <- mapping

  string "humidity-to-location map:" *> skipSpaces
  humidityToLocation <- mapping

  eof

  return $ Almanac
    seeds
    seedToSoil
    soilToFertilizer
    fertilizerToWater
    waterToLight
    lightToTemperature
    temperatureToHumidity
    humidityToLocation

part1 :: String -> (Location, Seed)
part1 input = minimum locations
  where
    almanac = fst $ head $ readP_to_S almanacP input

    m1 = almanac.seedToSoil
    m2 = almanac.soilToFertilizer
    m3 = almanac.fertilizerToWater
    m4 = almanac.waterToLight
    m5 = almanac.lightToTemperature
    m6 = almanac.temperatureToHumidity
    m7 = almanac.humidityToLocation

    locations = [(m7 !?! m6 !?! m5 !?! m4 !?! m3 !?! m2 !?! m1 !?! s, s)
                | s <- almanac.seeds]

seedRanges :: [Seed] -> [Seed]
seedRanges seeds
    = concat [[toEnum $ start+i | i <- [0..r-1]] | (idx, (start, r)) <- ys, even idx]
  where
    ss = fromEnum <$> seeds
    ys = zip [0..] $ zip ss (drop 1 ss)

part2 :: String -> (Location, Seed)
part2 input = minimum locations
  where
    almanac = fst $ head $ readP_to_S almanacP input

    m1 = almanac.seedToSoil
    m2 = almanac.soilToFertilizer
    m3 = almanac.fertilizerToWater
    m4 = almanac.waterToLight
    m5 = almanac.lightToTemperature
    m6 = almanac.temperatureToHumidity
    m7 = almanac.humidityToLocation

    seeds = seedRanges almanac.seeds

    locations = [(m7 !?! m6 !?! m5 !?! m4 !?! m3 !?! m2 !?! m1 !?! s, s)
                | s <- seeds]
