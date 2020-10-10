-- |

module Ch20 where
import           Control.Applicative
import           Data.Monoid
import           Data.Foldable

sumThem :: [a] -> [Sum a]
sumThem = fmap Sum -- map Sum

foldSum :: Num a => [a] -> Sum a
foldSum = fold . sumThem

prodThem :: [a] -> [Product a]
prodThem = fmap Product -- map  Product

foldProd :: Num a => [a] -> Product a
foldProd = fold . prodThem

xs :: [Sum Integer]
xs = [1, 2, 3, 4]

fs :: Sum Integer
fs = fold xs

ys :: [Product Integer]
ys = [1, 2, 3, 4]
fp :: Product Integer
fp = fold ys

-- foldMap ::   (a -> m) -> t a -> m
-- All :: Bool -> All

foldmapAll :: (Foldable t) => t Bool -> All
foldmapAll = foldMap All

data Identity a = Identity a

instance Foldable Identity where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldl :: (b -> a -> b) -> b -> t a -> b
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional x = Nada | Yep x
instance Foldable Optional where
  -- foldMap :: (Foldable t, Monoid m) => (a -> m a) -> t a -> m
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

maybesToList :: [Maybe a] -> [a]
maybesToList = concat . map toList

{-
chapter 20 exercises: Library functions
-}
-- sum :: (Foldable t, Num a) => t a -> a
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

-- prod :: (Foldable t, Num a) => t a -> a
prod' :: (Foldable t, Num a) => t a -> a
prod' = getProduct . (foldMap Product)
-- elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = getAny . (foldMap (fmap Any (== e)))

minimum'' :: (Ord a, Monoid a) => [a] -> a
minimum'' []       = mempty
minimum'' (x : xs) = foldl (\x y -> min x y) x xs

-- minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
-- foldl :: (b -> a -> b) -> b -> [a] -> b
minimum' = foldr (\x y -> min' x y) Nothing
 where
  min' :: (Ord a) => a -> (Maybe a) -> Maybe a
  min' x Nothing  = Just x
  min' x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\x y -> max' x y) Nothing
 where
  max' :: (Ord a) => a -> (Maybe a) -> Maybe a
  max' x Nothing  = Just x
  max' x (Just y) = Just $ max x y

length' :: (Foldable t) => t a -> Int
length' = foldl (\x _ -> succ x) 0

null', null'' :: Foldable t => t a -> Bool
null' = (0 ==) . length'
null'' = foldl (\x _ -> (True || x)) False
