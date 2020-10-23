{-# language OverloadedStrings#-}
{- |
Module          : Text.Franctions
Description     :  From the Haskell Book chapter 23

-}

module Fractions where
import           Control.Applicative
import qualified Control.Monad                 as M
import           Data.Attoparsec.Text           ( parseOnly )
import           Data.Ratio                     ( (%) )
import           Data.String                    ( IsString )
import           Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

-- decimal :: Integral a => Parser a
virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseFraction :: (Monad m, MonadFail m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> M.fail ("Denominator cannot be zero" :: IsString s => s)
    _ -> return (numerator % denominator)

main3 :: IO ()
main3 = do
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad


  let pF' = parseString parseFraction mempty
  --     pF'' = parseString virtuousFraction mempty
  -- print $ pF'' shouldWork
  -- print $ pF'' shouldAlsoWork
  -- print $ pF'' alsoBad
  -- print $ pF'' badFraction

  print "+++++++++++++++++++++++++++"

  print $ pF' badFraction
  print $ pF' shouldWork
  print $ pF' shouldAlsoWork
  print $ pF' alsoBad
