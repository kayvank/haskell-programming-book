-- |
{-
Chapter 23, State Exercises
-}
module Ch23 where
import           Control.Monad
import           Control.Monad.Trans.State

get :: State s s
get = state (\x -> (x, x))

put :: s -> State s ()
put s = state (\x -> ((), s))

exec :: State s a -> s -> s
exec sa = let f = runState sa in snd . f

eval :: State s a -> s -> a
eval sa = let f = runState sa in fst . f

modify :: (s -> s) -> State s ()
modify f = state (\s -> ((), f s))
