-- |

module Parsers where
import           Data.Char

newtype Parser a = P (String -> [(a, String)] )
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P
  (\x -> case x of
    []       -> []
    (x : xs) -> [(x, xs)]
  )
instance Functor Parser where
  fmap g p = P
    (\inp -> case parse p inp of
      []         -> []
      [(v, out)] -> [(g v, out)]
    )
instance Applicative Parser where
  pure a = P (\x -> [(a, x)])
  -- Parser(a -> b) -> Parser a -> Parser b
  pg <*> px = P
    (\inp -> case parse pg inp of
      []         -> []
      [(g, out)] -> parse (fmap g px) out
    )

instance Monad Parser where
  -- (>>=) :: m a (a -> mb) -> m b
  pa >>= f = P
    (\inp -> case parse pa inp of
      []         -> []
      [(v, out)] -> parse (f v) out
    )


three, three' :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item where g x y z = (x, z)
three' = do
  x <- item
  item
  z <- item
  return (x, z)

class Applicative f => Alternative f where
  empty :: f  a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  many x = some x <|> pure []
  some :: f a -> f [a]
  some x = pure (:) <*> x <*> many x

instance Alternative Parser where
  empty = P (\x -> [])
  p <|> q = P
    (\inp -> case parse p inp of
      []         -> parse q inp
      [(v, out)] -> [(v, out)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

upper, lower, alphanum, digit, letter :: Parser Char
upper = sat isUpper
lower = sat isLower
digit = sat isDigit
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  xs <- many (sat isSpace)
  return ()

int :: Parser Int
int =
  do
      char '-'
      n <- nat
      return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token $ string xs

nats :: Parser [Int]
nats = do
  symbol "["
  n  <- natural
  ns <- many
    (do
      symbol ","
      natural
    )
  symbol "]"
  return (n : ns)
