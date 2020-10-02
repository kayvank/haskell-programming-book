-- | 

module Ch17 where

newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Functor Identity where
 -- fmap :: (a -> b) -> f a -> f b
  fmap g (Identity x) =   Identity (g x) 

instance Applicative Identity where
  -- pure ::  a -> f a
  pure a  =  Identity a
  -- (<*>) :: f (a -> b) -> f a -> f b 
  (<*>) (Identity g) (Identity x)  =  Identity (g x)

