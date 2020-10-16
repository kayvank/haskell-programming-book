{-# LANGUAGE InstanceSigs #-}

-- |

module ReaderPractice where
import           Control.Applicative
import           Data.Maybe
import           Prelude                 hiding ( lookup )

x, y, z :: Num a => [a]
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

isLabel :: Eq a => a -> (a, b) -> Bool
isLabel k = (== k) . fst

-- foldr :: Foldable t => (a -> b -> b ) -> b t a -> b
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup k = foldr (\a b -> if f a then Just $ snd a else b) Nothing
  where f = isLabel k
lookup' k = foldl (\a b -> if f b then Just $ snd b else a) Nothing
  where f = isLabel k

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer) -- xs, ys
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer) -- ys, zs
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z'', z'') where z'' = z' n

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt, bolt' :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)
bolt' = liftA2 (&&) (> 3) (< 8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a
