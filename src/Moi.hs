-- |

module Moi where
import           Data.Functor
import           Control.Monad

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

  -- fmap :: (a -> b) -> f a -> f b
  {- Andrew solution
   fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')
  -}
instance Functor (Moi s) where
  fmap f (Moi st) = Moi (\s -> let g = f . fst . st in ((g s), s))

  {-
pure :: a -> Moi s a
(<*>) :: f (a -> b) -> f a -> f b
         Moi s (a -> b) -> Moi s a -> Moi s b
-}
instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))
  (Moi f) <*> (Moi g) = Moi
    (\s ->
      let (h, _) = f s
          (a, _) = g s
      in  (h a, s)
    )

  {-
(>>=) :: m a -> ( a -> m b ) -> m b
         Moi s a  -> (a -> Moi s b) -> Moi s b
-}
instance Monad (Moi s) where
  return = pure
  Moi f >>= g = Moi
    (\s ->
      let (a, _) = f s
          moiB   = g a
      in  (runMoi moiB) s
    )
