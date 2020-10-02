module Ch22 where

import           Control.Applicative
import           Control.Monad
import           Data.Char                     as C


boop = (* 2)
doop = (+ 10)
bip = boop . doop

compose' = boop . doop

fmp = boop <$> doop

duwop = (+) <$> boop <*> doop

duwop' = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

composeA :: [Char] -> [Char]
composeA = (++) <$> rev <*> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  _w <- rev
  _u <- cap
  return (_w, _u)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = undefined

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp f g a = join $ f <$> (g a)
mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp' f g a = (g a) >>= f

