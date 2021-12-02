module Main where

import Data.Foldable
import Data.Text
import qualified Data.Text as T
import qualified Day1 (solve)
import Lib
import System.Clock
import System.Console.Pretty
import System.Directory
import TextShow

solutions = [Day1.solve]

toHuman :: TimeSpec -> Text
toHuman input = nanoToHuman $ toNanoSecs input

nanoToHuman :: Integer -> Text
nanoToHuman ns
  | ns >= 1_000_000 = show (fromIntegral ns / 1_000_000.0) <> "s"
  | ns >= 1_000 = show (fromIntegral ns / 1_000.0) <> "ms"
  | True = show ns <> "ns"

blue = colorize Foreground Blue

green = colorize Foreground Green

listDirectoryText :: Text -> IO [Text]
listDirectoryText dir = do
  results <- listDirectory dirT
  pure $ Prelude.map T.pack results
  where
    dirT = T.unpack dir

data PuzzleResult = PuzzleResult
  { solution :: Text,
    elapsedNano :: Integer
  }

main :: IO ()
main = do
  directory <- listDirectoryText "input/day1"
  printT $ sort directory

  putTextLn "================================="
  putTextLn "❄️❄️❄️ 🎄 Advent of Code 2021 🎄 ❄️❄️❄️"
  putTextLn "================================="

  putTextLn $ "- " `append` (blue "Day1")
  --setSGR [SetColor Foreground Vivid White]
  puzzleInput <- readFileText "input/day1/part1.txt"

  startTime <- getTime ProcessCPUTime
  let result = Day1.solve [puzzleInput]
  endTime <- getTime ProcessCPUTime

  putTextLn $ "    part 1: "
  putTextLn $ "      solution: " `append` result
  putTextLn $ "      took: " `append` (green (toHuman $ diffTimeSpec startTime endTime))

  pure ()
