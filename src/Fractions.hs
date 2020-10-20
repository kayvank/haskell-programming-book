{-# language OverloadedStrings#-}
{- |
Module          : Text.Franctions
Description     :  From the Haskell Book chapter 23

-}

module Fractions where
import           Control.Applicative
import           Data.Ratio                     ( (%) )
import           Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

-- decimal :: Integral a => Parser a
parseFraction, virtuousFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main3 :: IO ()
main3 = do
  let parseFraction'  = parseString parseFraction mempty
  let parseFraction'' = parseString virtuousFraction mempty
  print $ parseFraction'' shouldWork
  print $ parseFraction'' shouldAlsoWork
  print $ parseFraction'' alsoBad
  print $ parseFraction'' badFraction

  print "+++++++++++++++++++++++++++"

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
