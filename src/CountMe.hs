-- | 

module CountMe where

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  -- fmap :: (a -> b) ->   f a -> f b
  fmap f ( CountMe i a  ) = CountMe (i+1) (f a)

instance Applicative CountMe where
  pure a  =  CountMe 0 a
  -- (<*>)  :: f (a -> b) -> f a -> f b
  (<*>)  (CountMe i f) (CountMe j a) = CountMe (i+j)(f a)

instance Monad CountMe where
  return a = CountMe 0 a
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (CountMe i a) f =  f a
