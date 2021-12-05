module Day2 where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Void
import Text.Megaparsec (MonadParsec (label, takeWhile1P), Parsec, ParsecT, State, Tokens, between, chunk, manyTill, runParser, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, digitChar, numberChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import TextShow
import Prelude hiding (Down)

type Parser = Parsec Void Text

data Direction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

parseForward :: Parser Direction
parseForward = do
  symbol "forward"
  Forward <$> integer

parseDown :: Parser Direction
parseDown = do
  symbol "down"
  Down <$> integer

parseUp :: Parser Direction
parseUp = do
  symbol "up"
  Up <$> integer

parseDirection :: Parser Direction
parseDirection =
  parseUp
    <|> parseDown
    <|> parseForward

parseDirections :: Parser [Direction]
parseDirections =
  many parseDirection

sc :: Parser ()
sc = void $ Prelude.some (char ' ' <|> char '\t' <|> char '\n')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = lexeme . string

stepPosition :: (Int, Int) -> Direction -> (Int, Int)
stepPosition (hpos, depth) (Forward amount) = (hpos + amount, depth)
stepPosition (hpos, depth) (Down amount) = (hpos, depth + amount)
stepPosition (hpos, depth) (Up amount) = (hpos, depth - amount)

solvePart1 :: Text -> IO Text
solvePart1 input = do
  let directions = fromRight [] (runParser parseDirections "day2 - part1" input)

  let (hpos, depth) = foldl' stepPosition (0, 0) directions

  pure $ showt (hpos * depth)

stepPositionAndAim :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
stepPositionAndAim (hpos, depth, aim) (Forward amount) = (hpos + amount, depth + (aim * amount), aim)
stepPositionAndAim (hpos, depth, aim) (Down amount) = (hpos, depth, aim + amount)
stepPositionAndAim (hpos, depth, aim) (Up amount) = (hpos, depth, aim - amount)

solvePart2 :: Text -> IO Text
solvePart2 input = do
  let directions = fromRight [] (runParser parseDirections "day2 - part2" input)

  let (hpos, depth, aim) = foldl' stepPositionAndAim (0, 0, 0) directions

  pure $ showt (hpos * depth)
