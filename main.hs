
-- it is working for me
-- yay! :-D
-- hello there!
-- me too, kayvan

-- ╭( ･ㅂ･)و


-- in Stack, the file is called package.yml
{-
dependencies:
- base >= 4.7 && < 5
- time
- transformers
- random
- dlist
-}


import System.Random ( Random(randomR), StdGen )

import Control.Monad.Trans.State ( execState, get, put, State )

import qualified Data.DList                    as DL


-- Six-sided die
data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- 1.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen


-- 2. Change rollsToGetN to record the series of dice that are rolled, in addition to the count of the total number of rolls:

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (i, list)
 where
  list       = DL.apply dlist []
  (i, dlist) = go 0 DL.empty 0 g

  go :: Int -> DL.DList Die -> Int -> StdGen -> (Int, DL.DList Die)
  go sum rolls count gen
    | sum >= n
    = (count, rolls)
    | otherwise
    = let (i, nextGen) = randomR (1, 6) gen
          die          = intToDie i
      in  go (sum + i) (DL.snoc rolls die) (count + 1) nextGen

---changwoo
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, log) gen
          | sum >= n = (count, log)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1, intToDie die : log) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap a2b (Moi s2as) = Moi $ (\(a, s) -> (a2b a, s)) . s2as

-- Andrew
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

-- Test


-- hack
instance Show (Moi s a) where
  show _ = "Moi { s -> (a, s) }"

-- hack
instance (Monoid s, Eq a, Eq s) => Eq (Moi s a) where
  (Moi f1) == (Moi f2) = f1 mempty == f2 mempty

instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Moi s a) where
  arbitrary = Moi <$> arbitrary

instance (Eq a, Eq s, Monoid s) => EqProp (Moi s a) where
  (=-=) = eq

    it "has a valid functor instance"
      $ let trigger = undefined :: Moi [Int] (Int, Int, Int)
        in  quickBatch $ functor trigger


-- State Applicative
-- Write the Applicative instance for State

{-# LANGUAGE TupleSections #-}

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a, )
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (f', s' ) = f s
        (a , s'') = g s'
    in  (f' a, s'')

{-

https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-204

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

-}

-- Test

  describe "Write the Applicative instance for State"
    $ it "has a valid applicative instance"
    $ let trigger = undefined :: Moi String (String, String, Int)
      in  quickBatch $ applicative trigger


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b 
  (Moi s2as) >>= a2Moi = Moi $ \s -> 
    let
      (a1, s1) = s2as s
      s2bs = a2Moi a1
      bs = runMoi s2bs $ s1
    in
      bs


      
instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, s') = f s
        moisb   = g a
        res     = runMoi moisb s'
    in  res

-- https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-221

instance (Monad m) => Monad (StateT s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = StateT $ \ s -> return (a, s)
    {-# INLINE return #-}
#endif

    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail str = StateT $ \ _ -> fail str
    {-# INLINE fail #-}
#endif




-- FizzBuzz differently
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put $ result : xs

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [to, (to - 1) .. from]

get' :: State s s
get' = StateT $ \s -> Identity (s, s)
-- get' = state $ \s -> (s, s)

putMoi :: s -> Moi s () 
putMoi s = Moi (\_ -> ((), s))

-- Andrew
put' s = Moi $ const ((), s)

-- 3. Run the State with s and get the state that results
exec' :: Moi s a -> s -> s
exec' (Moi sa) = snd . sa

evalMoi :: Moi s a -> s -> a
evalMoi (Moi s2as) = fst . s2as

modify'' :: (s -> s) -> State s ()
modify'' f = state $ \s -> ((), f s)
-- runMoi f >> f 0

modify' :: (s -> s) -> Moi s ()
modify' f = do
  s <- get'
  let modifiedS = f s
  put' modifiedS 
  

modify'' :: (s -> s) -> Moi s ()
modify'' f = get' >>= put' . f

--changwoo test.
    it "modify" $ do
      let f = modify'' (+1)
      runState f 0 `shouldBe` ((), 1)
      (runState (f >> f) 0) `shouldBe` ((),2)



instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = join (Reader $ \r -> aRb (ra r))
