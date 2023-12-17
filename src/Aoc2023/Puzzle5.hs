{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Aoc2023.Puzzle5 where

import Lib.ReadP

import Control.Applicative hiding (many, optional)
import Data.Bifunctor
import Data.Functor
import Data.Map.Lazy (Map)
import Data.Maybe
import Text.ParserCombinators.ReadP

import qualified Data.Map.Lazy as Map


newtype Seed = Seed Int deriving (Read, Show, Eq, Ord, Enum)
newtype Soil = Soil Int deriving (Read, Show, Eq, Ord, Enum)
newtype Fertilizer = Fertilizer Int deriving (Read, Show, Eq, Ord, Enum)
newtype Water = Water Int deriving (Read, Show, Eq, Ord, Enum)
newtype Light = Light Int deriving (Read, Show, Eq, Ord, Enum)
newtype Temperature = Temperature Int deriving (Read, Show, Eq, Ord, Enum)
newtype Humidity = Humidity Int deriving (Read, Show, Eq, Ord, Enum)
newtype Location = Location Int deriving (Read, Show, Eq, Ord, Enum)

data Almanac = Almanac
  { seeds :: [Seed]
  , seedToSoil :: (Map Seed Soil)
  , soilToFertilizer :: (Map Soil Fertilizer)
  , fertilizerToWater :: (Map Fertilizer Water)
  , waterToLight :: (Map Water Light)
  , lightToTemperature :: (Map Light Temperature)
  , temperatureToHumidity :: (Map Temperature Humidity)
  , humidityToLocation :: (Map Humidity Location)
  }
  deriving (Read, Show)


fillWithDefault :: (Ord a, Enum a, Enum b) => [a] -> Map a b -> Map a b
fillWithDefault xs m
  = m `Map.union` Map.fromList [(x, toEnum $ fromEnum x) | x <- xs]

-- | Compose two maps inserting default values, if required.
(<.>) :: (Ord a, Ord b, Enum b, Enum c) => Map a b -> Map b c -> Map a c
m1 <.> m2 = Map.compose m2' m1
  where
    m2' = fillWithDefault (Map.elems m1) m2


-- | Map lookup with default using enum instances
infixr 6 !?!
(!?!) :: (Ord a, Enum a, Enum b) => Map a b -> a -> b
m !?! k = Map.lookup k m `orElse` (toEnum $ fromEnum k)
  where
    x `orElse` y = maybe y id x

-- | Parses (dest, src, range) information into a list of pairs (src, dest).
--
-- Examples:
--
-- >>> fst $ head $ readP_to_S (mapping <* eof) "52 50 10"
-- [(50,52),(51,53),(52,54),(53,55),(54,56),(55,57),(56,58),(57,59),(58,60),(59,61)]
-- >>> result = fst $ head $ readP_to_S (mapping <* eof) "50 98 2\n52 50 48"
-- >>> result == [(98+i,50+i) | i <- [0..1]] ++ [(50+i,52+i) | i <- [0..47]]
-- True
mapping :: ReadP [(Int, Int)]
mapping = do
  xs <- many $ do
    dest <- natural
    string " " *> skipSpaces
    src <- natural
    string " " *> skipSpaces
    range <- natural
    optional newline
    return $ zip [src .. src + range - 1] [dest .. dest + range - 1]
  
  skipSpaces
  return $ concat xs

fromMapping :: (Ord a, Enum a, Enum b) => ReadP (Map a b)
fromMapping = fmap (bimap toEnum toEnum) <$> mapping <&> Map.fromList

almanacP :: ReadP Almanac
almanacP = do
  seeds <- string "seeds: " *> ((fmap Seed) <$> natural `sepBy` space)
  skipSpaces

  string "seed-to-soil map:" *> skipSpaces
  seedToSoil <- fromMapping

  string "soil-to-fertilizer map:" *> skipSpaces
  soilToFertilizer <- fromMapping

  string "fertilizer-to-water map:" *> skipSpaces
  fertilizerToWater <- fromMapping

  string "water-to-light map:" *> skipSpaces
  waterToLight <- fromMapping

  string "light-to-temperature map:" *> skipSpaces
  lightToTemperature <- fromMapping

  string "temperature-to-humidity map:" *> skipSpaces
  temperatureToHumidity <- fromMapping

  string "humidity-to-location map:" *> skipSpaces
  humidityToLocation <- fromMapping

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

-- | Create a function from a map using defaults from `def`
-- | if the given key doesn't exist.
fromMap :: (a -> b) -> Map a b -> (a -> b)
fromMap def m = undefined

-- part1 :: String -> Int
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

part2 :: String -> Int
part2 = undefined
