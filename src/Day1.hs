module Day1 where

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
  ( case depth > lastDepth && lastDepth /= 0 of
      True -> (depth, count + 1)
      False -> (depth, count)
  )

solve :: [Text] -> Text
solve [part1] =
  part1
    & T.lines
    & inputToNumbers
    & Prelude.flipfoldl' reduction (0, 0)
    & snd
    & showt
