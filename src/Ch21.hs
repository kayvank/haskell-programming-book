-- |

module Ch21 where
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Functor

import           Data.ByteString.Lazy    hiding ( map )
import           Network.Wreq
import           Prelude                 hiding ( )
import           Data.Traversable

{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f ( t a )
  traverse f = sequenceA . fmap f
-}

{-
traverse :: (Applicative f, Traversable t) =>
            (a -> f b) -> t a -> f ( t a )
fmap        (a -> b) -> f a -> f b
(=<<)       (a -> m b) -> m a -> m b
traverse    (a -> f b) -> t a -> f (t b)
-}
f :: (Num a, Ord a) => a -> Maybe a
f x | x < 10    = Just x
    | otherwise = Nothing

withMap, withTraverse :: (Num a, Ord a) => [a] -> Maybe [a]
withMap = sequenceA . (fmap f) -- sequenceA $ fmap f
withTraverse = traverse f

type Err = String
fe :: (Num a, Ord a) => a -> Either Err a
fe x | x < 10    = Right x
     | otherwise = Left "it is not 10!!!"

efeTrav :: (Num a, Ord a) => [a] -> Either Err [a]
efeTrav = traverse fe

efe :: (Num a, Ord a) => [a] -> [Either Err a]
efe = fmap fe

type Morse = String

charToMorse :: Char -> Maybe Morse
charToMorse = undefined

moreseToChar :: Morse -> Maybe Char
moreseToChar = undefined


stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

morse :: String -> [Morse]
morse = (fromMaybe []) . stringToMorse

decodeToChar, decodeToChar' :: [Morse] -> Maybe String
decodeToChar = sequenceA . (fmap moreseToChar)
decodeToChar' = traverse moreseToChar

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined


pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

urls :: [String]
urls = ["http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mappingGet :: [IO (Response ByteString)]
mappingGet = fmap get urls

mappingGet' :: IO [(Response ByteString)]
mappingGet' = traverse get urls

{-
Traversable instances
-}
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left'  x) = Left' x
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' e) where
  pure = Right'
  Left'  e <*> _ = Left' e
  Right' f <*> r = f <$> r

instance Foldable (Either' a) where
  -- foldMap :: (Foldable t, Momoid m) (a -> m) ->t a -> m
  foldMap _ (Left'  _) = mempty
  foldMap f (Right' y) = f y

instance Traversable (Either' a) where
  -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f t a
  traverse _ (Left'  x) = pure $ Left' x
  traverse h (Right' x) = fmap Right' (h x)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree  where
  fmap _ Empty        = Empty
  fmap f (Node l x r) = Node (f <$> l) (f x) (f <$> r)

instance Applicative Tree where
  pure x = Node Empty x Empty
  Empty          <*> _            = Empty
  _              <*> Empty        = Empty
  (Node fl f fr) <*> (Node l x r) = Node (fl <*> l) (f x) (fr <*> r)

instance Foldable Tree where
{-
        foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
        foldMap _ [] =  mempty
        foldMap f (x:xs) =  (f x ) mappend foldMap f xs
-}

  foldMap _ Empty        = mempty
  foldMap f (Node l x r) = (f x) <> (foldMap f l) <> (foldMap f r)

instance Traversable Tree where
{-
  traverse :: (Applicative f, Traverseable t) => (a -> f b) -> t a -> f t b
  sequenceA :: (Applicative f, Traverseable t) => t (f a) -> f (t a)
-}
  sequenceA Empty        = pure Empty
  sequenceA (Node l x r) = Node <$> (sequenceA l) <*> x <*> (sequenceA r)
  traverse f = sequenceA . (fmap f)
fff = (\x -> id x)
