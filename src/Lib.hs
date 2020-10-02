module Lib
    ( someFunc
    ) where
import Control.Monad(join)
import Ch22

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bind :: Monad m => (a -> m b) -> m a -> m b
-- bind  f m =  join $ f <$> m
bind  f m = f' m  where
  f'  = join . (fmap f)
