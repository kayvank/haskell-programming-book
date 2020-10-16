{-# language InstanceSigs #-}

module Ch22 where

import           Control.Applicative
import           Control.Monad
import           Data.Char                     as C
import           Data.Functor
import           Control.Monad.Reader


boop, doop, bip, compose', fmp :: Num a => a -> a
boop = (* 2)
doop = (+ 10)

bip = boop . doop
compose' = boop . doop
fmp = boop <$> doop

{- lift (+) over partially applied context, functions -}

--- (<*> :: f(a -> b) -> f a -> f b)
appReader :: (a -> a -> b) -> (a -> a) -> (a -> b)
appReader = (<*>)

duwop, duwop' :: Num a => a -> a
duwop = (+) <$> boop <*> doop
duwop' = liftA2 (+) boop doop

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)


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

newtype Reader' r a = Reader' {runReader' :: r -> a}
instance Functor (Reader' r) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap f (Reader' ra) = Reader' $ f . ra
  fmap f ra = let g = runReader' ra in Reader' $ f . g


ask :: Reader' a a
ask = Reader' $ id

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp f g a = join $ f <$> (g a)
mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
mcomp' f g a = (g a) >>= f

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

getDogR'' :: Reader' Person Dog
getDogR'' = Reader' (\p -> Dog (dogName p) (address p))

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f g h = f <$> g <*> h

asks :: (r -> a) -> Reader' r a
asks f = Reader' (\x -> (f x))

instance Applicative (Reader' r) where
  -- pure ::  a -> Reader r a
  pure a = Reader' (\x -> a)
  -- (<*>) :: f (a -> b) f a -> f b
  (Reader' rab) <*> (Reader' ra) = Reader' (\x -> rab x (ra x))

foo :: (Functor f, Num a) => f a -> f a
foo r = (+ 1) <$> r

bar :: (Foldable f) => t -> f a -> (t, Int)
bar r t = (r, length t)

barOne :: (Foldable f) => f a -> (f a, Int)
barOne r = (r, length r)

barPlus, barPlus', barPlus''
  :: (Foldable f, Functor f, Num a) => f a -> (f a, Int)
barPlus r = ((foo r), length r)
barPlus' = (\r -> ((foo r), length r))
barPlus'' = (\r -> bar (foo r) r)

footBind k m = (\r -> k (m r) r)
footBind k m = (\r -> k (m r) r)

footBind' m k = (\r -> k (m r) r)
footBind' m k = (\r -> k (m r) r)

getDogRm :: Person -> Dog
getDogRm = do
  n <- dogName
  a <- address
  return $ Dog n a

instance Monad (Reader' r) where
  -- return :: a -> Reader' r a
  return a = Reader' (\x -> a)
  -- (>>=) =  m a -> (a -> m b) -> m b
  Reader' ra >>= aRb = join $ Reader' (\r -> aRb (ra r))

{-
data Reader' r a = Reader { runReader:: r -> a }
Reader' :: (r -> a) -> Reader' r a
runReader ::  Reader r a -> r -> a
ReaderT :: (r -> m a) -> ReaderT r m a
runReaderT :: ReaderT r m a -> r -> m a
-}
withReaderT' :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT' f m = ReaderT $ runReaderT m . f
