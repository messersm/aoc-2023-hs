module Main (main) where

import qualified Aoc2023.Puzzle1 as Puzzle1
import qualified Aoc2023.Puzzle2 as Puzzle2
import qualified Aoc2023.Puzzle3 as Puzzle3

import System.Environment
import System.Exit
import System.IO

puzzles :: [(String -> String, String -> String)]
puzzles =
  [ (show . Puzzle1.part1, show . Puzzle1.part2)
  , (show . Puzzle2.part1, show . Puzzle2.part2)
  , (show . Puzzle3.part1, show . Puzzle3.part2)
  ]

main :: IO ()
main = do
  args <- getArgs

  case args of
    [n] -> do
      let puzzle = read n :: Int
      let filename = "puzzles/puzzle" ++ show puzzle ++ ".txt"

      if puzzle < 1 || puzzle > length puzzles
      then hPutStrLn stderr $ "Puzzle '" ++ n ++ "' does not exist or is not solved."
      else do
        let (part1, part2) = puzzles !! (puzzle-1)
        content <- readFile filename

        putStrLn $ "Puzzle " ++ (show puzzle)
        putStrLn $ " - Part 1: " ++ (part1 content)
        putStrLn $ " - Part 2: " ++ (part2 content)

    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <puzzle number>"
      exitFailure
