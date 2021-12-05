{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Foldable
import qualified Data.Map as Map
import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Day1
import qualified Day2
import qualified Day3
import Lib
import System.Clock
import System.Console.Pretty
import System.Directory
import TextShow

toHuman :: TimeSpec -> Text
toHuman input = nanoToHuman $ toNanoSecs input

nanoToHuman :: Integer -> Text
nanoToHuman ns
  | ns >= 1_000_000_000 = show (fromIntegral ns / 1_000_000_000.0) <> "s"
  | ns >= 1_000_000 = show (fromIntegral ns / 1_000_000.0) <> "ms"
  | ns >= 1_000 = show (fromIntegral ns / 1_000.0) <> "μs"
  | otherwise = show ns <> "ns"

blue = colorize Foreground Blue

yellow = colorize Foreground Yellow

green = colorize Foreground Green

listDirectoryText :: Text -> IO [Text]
listDirectoryText dir = do
  results <- listDirectory dirT
  pure $ Prelude.map T.pack results
  where
    dirT = T.unpack dir

data PuzzleResult = PuzzleResult
  { solution :: Text,
    duration :: TimeSpec
  }
  deriving (Show)

type PuzzleResults = Map Int [PuzzleResult]

readPuzzleInput :: (Int, Int) -> IO ((Int, Int), Text)
readPuzzleInput (day, part) = do
  text :: Text <- readFileText (buildPath day part)
  pure ((day, part), text)
  where
    buildPath :: Int -> Int -> String
    buildPath day part =
      T.unpack $
        foldl
          append
          ""
          [ "input/day",
            showt day,
            "/part",
            showt part,
            ".txt"
          ]

readPuzzleInputs :: [(Int, Int)] -> IO (Map (Int, Int) Text)
readPuzzleInputs inputs = do
  output :: [((Int, Int), Text)] <- mapM readPuzzleInput inputs
  pure $ Map.fromList output

-- | Given a list of puzzle inputs, and solvers indexed by day and part index,
-- | solves those puzzles and returns the results.
solve ::
  Map (Int, Int) Text ->
  -- | Puzzle Inputs
  Map (Int, Int) (Text -> IO Text) ->
  -- | Solvers
  IO (Map (Int, Int) PuzzleResult) -- Results
solve inputs solvers = do
  results <-
    inputs
      & Map.toList
      & mapM
        ( \((day, part), puzzleInput) -> do
            let solver :: (Text -> IO Text) = solvers Map.! (day, part)

            startTime <- getTime ProcessCPUTime
            result <- solver puzzleInput
            endTime <- result `deepseq` getTime ProcessCPUTime

            pure
              ( (day, part),
                PuzzleResult
                  { solution = result,
                    duration = diffTimeSpec startTime endTime
                  }
              )
        )

  pure $ Map.fromList results

solvers :: Map (Int, Int) (Text -> IO Text)
solvers =
  Map.fromList
    [ ((1, 1), Day1.solvePart1),
      ((1, 2), Day1.solvePart2),
      ((2, 1), Day2.solvePart1),
      ((2, 2), Day2.solvePart2),
      ((3, 1), Day3.solvePart1),
      ((3, 2), Day3.solvePart2)
    ]

main :: IO ()
main = do
  inputs <- readPuzzleInputs [(1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (3, 2)]
  solutions <- solve inputs solvers

  putTextLn "================================="
  putTextLn "❄️❄️❄️ 🎄 Advent of Code 2021 🎄 ❄️❄️❄️"
  putTextLn "================================="

  let list :: [((Int, Int), PuzzleResult)] = Map.toList solutions

  mapM_
    ( \((day, part), puzzleResult) -> do
        putTextLn $ blue $ foldl append "" ["• Day ", showt day, " 🠪 Part ", showt part]
        putTextLn $ "      solution: " `append` yellow (solution puzzleResult)
        putTextLn $ "      took: " `append` green (toHuman (duration puzzleResult))
        putTextLn ""
    )
    list