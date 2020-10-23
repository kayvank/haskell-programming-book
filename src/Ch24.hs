-- |
module Ch24 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Ratio ((%))
import Fractions
import Text.Trifecta

intOnly :: Parser Integer
intOnly = integer <* eof

testIntOnly = parseString intOnly mempty "123abc"

data MyName = MyName String deriving (Show)

someLetter :: Parser String
someLetter = some letter :: Parser [Char]

somedigits :: Parser [Integer]
somedigits = some integer :: Parser [Integer]

rationalOrInt :: Parser (Either Rational Integer)
rationalOrInt = do
  n <- decimal
  d <- Left <$> (char '/' *> decimal) <|> return (Right n)
  ratInt <- case d of
    Left m -> return (Left (n % m))
    otherwise -> return (Right n)
  return ratInt

{-
Chapter exercises
1- Parser for semantic versioning
-}
data NumberOrString
  = NOSS String
  | NOSI Integer

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  (NOSS <$> some letter <|> NOSI <$> integer)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  minor <- integer
  patch <- integer
  release <- some parseNumberOrString
  metadata <- some parseNumberOrString
  return $ SemVer major minor patch release metadata

parseDigit :: Parser Char
parseDigit = do
  n <- alphaNum
  if isDigit n then return n else empty

base10Integer :: Parser Integer
base10Integer = do
  ds <- (some parseDigit)
  num <-
    return
      ( let ps =
              (foldl (\a e -> a + (snd e) * 10 ^ (fst e)) 0)
                . (zip [0 .. (length ds) -1])
                . (fmap (toInteger . digitToInt))
         in ps ds
      )
  return num
