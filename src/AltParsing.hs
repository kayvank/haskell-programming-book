{-# language QuasiQuotes #-}

module AltParsing where

import           Control.Applicative
import           Text.Trifecta
import           Text.RawString.QQ

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos, parseNos' :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n") >> Left <$> integer <|> Right <$> (some letter)
parseNos' = skipMany (oneOf "\n") >> parseNos

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

main5 :: IO ()
main5 = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p (integer) b
  print $ p (parseNos) a
  print $ p (parseNos) b
  print $ p (many parseNos) c --many: zero or more
  print $ p (some parseNos) c -- some: one or more
  print $ p (some parseNos) eitherOr -- quasiquote

{-
results are:
Haskell λ: main5
Success "blah"
Success 123
Success (Right "blah")
Success (Left 123)
Success [Left 123,Right "blah",Left 789]
Success [Left 123,Right "blah",Left 789]
Failure (ErrInfo {_errDoc = (interactive):1:1: error: expected: integer, letter, _errDeltas = [Columns 0 0]})
-}
