-- |

module FizzBuzz where
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList                    as D

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizBuzzFromTo :: Integer -> Integer -> D.DList String
fizBuzzFromTo from to = fizBuzzFromTo' (from, to)
fizBuzzFromTo' :: (Integer, Integer) -> D.DList String
fizBuzzFromTo' (from, to) = (fizBuzzList . f) (from, to)
  where f x = [(fst x) .. (snd x - 1)]

fizBuzzList :: [Integer] -> D.DList String
fizBuzzList list = execState (mapM_ addResult list) D.empty


{-
  State s a :: State {runState :: s -> (a, s)}
  State   :: State {runState :: [  ] -> ( (  ) , [  ])}
-}
addResult, addResult' :: Integer -> State (D.DList String) ()
addResult n = do
  let z = fizzBuzz n
  state (\s -> ((), D.snoc s z))

addResult' n =
  state (\s -> let fizbuz = fizzBuzz n in ((), (D.snoc s fizbuz)))

main1 :: IO ()
main1 = mapM_ (putStrLn . fizzBuzz) [1 .. 100]
