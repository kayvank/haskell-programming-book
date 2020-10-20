-- |

module Exp where

import           Parsers

expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return (t + e) <|> return t

factor :: Parser Int
factor = do
  symbol "("
  e <- expr
  symbol ")"
  return e <|> nat


term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f * t) <|> return f
