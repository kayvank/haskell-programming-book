-- |

module RandomExample2 where
import           Control.Applicative
import           Control.Monad                  ( replicateM )
import           Control.Monad.Trans
import           Control.Monad.State
import           System.Random
import           RandomExample
import qualified Data.DList                    as D

{-
take a state like function and embeded in a monad transformer
state :: Monad m => (s -> (a, s)) -> StateT s m a
-}
rollDie, rollDie' :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

inifinitDie, inifinitDie' :: State StdGen [Die]
inifinitDie = repeat <$> rollDie'
inifinitDie' = replicateM 3 rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1) nextGen

rollsToGetN, rollsToGetN' :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1) nextGen
--TODO use replicateM veriation to replicate conditionally
rollsToGetN' = undefined

rollsCountLogged :: Int -> StdGen -> (Int, D.DList Die)
rollsCountLogged n g = go 0 (0, D.empty) g where
  go :: Int -> (Int, D.DList Die) -> StdGen -> (Int, D.DList Die)
  go sum (count, dies) gen
    | sum >= n
    = (count, dies)
    | otherwise
    = let (die, nextGen) = (randomR (1, 6) gen)
          intCons i dlist = D.cons (intToDie i) dlist
      in  go (sum + die) (count + 1, (intCons die dies)) nextGen

rollsCountLogged' :: Int -> Int -> (Int, D.DList Die)
rollsCountLogged' targetSum = (rollsCountLogged targetSum) . mkStdGen

randomRollIO :: Int -> IO (Int, D.DList Die)
randomRollIO targetSum = (rollsCountLogged' targetSum) <$> randomIO
