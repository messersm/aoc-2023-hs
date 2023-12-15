module Aoc2023.Puzzle1 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Text.ParserCombinators.ReadP


readDigit :: ReadP Integer
readDigit
   =  (string "one"   >> return 1)
  <|> (string "two"   >> return 2)
  <|> (string "three" >> return 3)
  <|> (string "four"  >> return 4)
  <|> (string "five"  >> return 5)
  <|> (string "six"   >> return 6)
  <|> (string "seven" >> return 7)
  <|> (string "eight" >> return 8)
  <|> (string "nine"  >> return 9)
  <|> (satisfy isDigit <&> (read . pure))

someDigit :: ReadP Integer
someDigit = do
  skipMany get
  digit <- readDigit
  skipMany get
  eof
  return digit

part1 :: String -> Integer
part1 content = sum $ do
  line <- lines content
  let ns = (read . pure) <$> filter isDigit line
  return $ 10 * head ns + last ns

part2 :: String -> Integer
part2 content = sum $ do
  line <- lines content
  let ns = [xs | (xs, _) <- readP_to_S someDigit line]
  return $ 10 * head ns + last ns
