{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
module IniParser where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

type Name = String

type Value = String

type Assignment = Map Name Value

newtype Header = Header String deriving (Eq, Ord, Show)

data Section = Section Header Assignment deriving (Eq, Show)

newtype Config = Config (Map Header Assignment) deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parserHeader :: Parser Header
parserHeader = parseBracketPair (Header <$> some letter)

parserAssignment :: Parser (Name, Value)
parserAssignment = do
  k <- some letter
  char '='
  v <- some (noneOf "\n")
  skipEOL
  return (k, v)

parserAssignment' :: Parser Assignment
parserAssignment' = do
  (k, v) <- parserAssignment
  return (M.singleton k v)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- | Skip comments starting at the
--  beginning of the line.
skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL
    )

parseSection :: Parser Section
parseSection = do
  skipWhiteSpace
  skipComments
  h <- parserHeader
  skipEOL
  m <- some parserAssignment'
  return $ Section h (F.fold m)

rollup :: Section -> Map Header Assignment -> Map Header Assignment
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

parseDigit :: Parser Char
parseDigit = digit

base10Integer :: Parser Integer
base10Integer = undefined
