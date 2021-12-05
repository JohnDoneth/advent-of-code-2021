module Day1 where

import Data.Text (Text)
import qualified Data.Text as T
import TextShow

textToInt :: Text -> Int
textToInt input =
  input
    & T.unpack
    & readMaybe
    & fromMaybe 0

inputToNumbers :: [Text] -> [Int]
inputToNumbers = map textToInt

reduction :: Int -> (Int, Int) -> (Int, Int)
reduction depth (lastDepth, count) =
  if depth > lastDepth && lastDepth /= 0
    then (depth, count + 1)
    else (depth, count)

solvePart1 :: Text -> IO Text
solvePart1 input =
  input
    & T.lines
    & inputToNumbers
    & Prelude.flipfoldl' reduction (0, 0)
    & snd
    & showt
    & pure

windowed :: Int -> [a] -> [[a]]
windowed size ls =
  case ls of
    [] -> []
    x : xs ->
      if length ls >= size
        then take size ls : windowed size xs
        else windowed size xs

solvePart2 :: Text -> IO Text
solvePart2 input =
  input
    & T.lines
    & inputToNumbers
    & windowed 3
    & map sum
    & Prelude.flipfoldl' reduction (0, 0)
    & snd
    & showt
    & pure
