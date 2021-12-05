{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (digitToInt)
import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty (NonEmpty (..), map, transpose)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Void
import Text.Megaparsec (MonadParsec (label, takeWhile1P), Parsec, ParsecT, State, Tokens, between, chunk, manyTill, runParser, sepBy, some, someTill, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, digitChar, numberChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Stream (Token (..))
import TextShow
import Prelude hiding (Down, map, transpose)

type Parser = Parsec Void Text

parseBinaryDigit :: Parser Bool
parseBinaryDigit = do
  x :: Char <- char '1' <|> char '0'
  pure $ case x of
    '1' -> True
    '0' -> False

parseRow :: Parser (NonEmpty Bool)
parseRow = many parseBinaryDigit

parseRows :: Parser (NonEmpty (NonEmpty Bool))
parseRows = sepBy parseRow (char '\n')

mostCommon :: NonEmpty (NonEmpty Bool) -> (NonEmpty Bool)
mostCommon = map common

leastCommon :: NonEmpty (NonEmpty Bool) -> (NonEmpty Bool)
leastCommon = map (not . common)

common :: (NonEmpty Bool) -> Bool
common input = countTrue input > countFalse input
  where
    countTrue = length . filter (== True)
    countFalse = length . filter (== False)

binFromBools :: (NonEmpty Bool) -> Int
binFromBools = foldl' (\acc x -> acc * 2 + boolToInt x) 0
  where
    boolToInt True = 1
    boolToInt False = 0

gamma :: NonEmpty (NonEmpty Bool) -> Int
gamma input = input & transpose & mostCommon & binFromBools

epsilon :: NonEmpty (NonEmpty Bool) -> Int
epsilon input = input & transpose & leastCommon & binFromBools

solvePart1 :: Text -> IO Text
solvePart1 input = do
  let rows = fromRight (nonEmpty [False]) (runParser parseRows "day3 - part1" input)

  let g = gamma rows
  let e = epsilon rows

  pure $ showt (g * e)

oxygen :: NonEmpty (NonEmpty Bool) -> (NonEmpty Bool)
oxygen = firsts
  where
    mapper :: NonEmpty (NonEmpty Bool) -> (NonEmpty Bool)
    mapper = map (head 0)

    firsts :: NonEmpty (NonEmpty Bool) -> (NonEmpty Bool)
    firsts input = input & transpose & mapper

solvePart2 :: Text -> IO Text
solvePart2 input = do
  let rows = fromRight (nonEmpty [False]) (runParser parseRows "day3 - part1" input)

  let o = oxygen rows
  --let e = epsilon rows

  pure $ showt o
