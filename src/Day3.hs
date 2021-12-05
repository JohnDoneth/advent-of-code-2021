{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (digitToInt)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Void
import Text.Megaparsec (MonadParsec (label, takeWhile1P), Parsec, ParsecT, State, Tokens, between, chunk, manyTill, runParser, sepBy, some, someTill, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, digitChar, numberChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Stream (Token (..))
import TextShow
import Prelude hiding (Down)

type Parser = Parsec Void Text

newtype Epsilon = Epsilon Int -- LE

newtype Gamma = Gamma Int -- BE

-- fromLittleEndian :: Text -> Int
-- fromLittleEndian = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- fromBigEndian :: Text -> Int
-- fromBigEndian = foldr (\x acc -> acc * 2 + digitToInt x) 0

sc :: Parser ()
sc = void $ Prelude.some (char ' ' <|> char '\t' <|> char '\n')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = lexeme . string

parseBinaryDigit :: Parser Bool
parseBinaryDigit = do
  x :: Char <- char '1' <|> char '0'
  pure $ case x of
    '1' -> True
    '0' -> False

parseRow :: Parser [Bool]
parseRow = many parseBinaryDigit

parseRows :: Parser [[Bool]]
parseRows = sepBy parseRow (char '\n')

mostCommon :: [[Bool]] -> [Bool]
mostCommon = map common

leastCommon :: [[Bool]] -> [Bool]
leastCommon = map (not . common)

common :: [Bool] -> Bool
common input = countTrue input > countFalse input
  where
    countTrue = length . filter (== True)
    countFalse = length . filter (== False)

binFromBools :: [Bool] -> Int
binFromBools = foldl' (\acc x -> acc * 2 + boolToInt x) 0
  where
    boolToInt True = 1
    boolToInt False = 0

gamma :: [[Bool]] -> Int
gamma input = input & Prelude.transpose & mostCommon & binFromBools

epsilon :: [[Bool]] -> Int
epsilon input = input & Prelude.transpose & leastCommon & binFromBools

solvePart1 :: Text -> IO Text
solvePart1 input = do
  let rows = fromRight [] (runParser parseRows "day3 - part1" input)

  let g = gamma rows
  let e = epsilon rows

  pure $ showt (g * e)
