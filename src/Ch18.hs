-- | 

module Ch18 where
import Control.Monad
import Control.Applicative

-- fmap a -> fa -> fb -> f f b
addOne :: Num a => a  -> [a]
addOne x = [x, 1]

func1 :: Num a => (a -> [a]) -> [a] -> [[a]]
func1  f l = f <$> l

result = func1 addOne [1,2,3]

bind' :: Monad m => ( a -> m b ) -> m a -> m b
bind' f  =  join . fmap f 

-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Applicative M => (a1 -> r) -> m a -> f b
-- fmap :: ( a-> b ) f a -> f b
fmap' :: [Int]
fmap' = fmap (\x -> x*2) [1,2,3]

liftA' :: [Int]
liftA' = liftA(\x -> x*2) [1,2,3]

liftM' :: [Int]
liftM' =  liftM (\x -> x*2) [1,2,3]

{-
liftA2 :: Applicateive f => (a -> b -> c ) -> f a -> f b  -> f c
lifts a binary function, g, accros two applicatives, each having a parameter of function g
-}

just_11'' = liftA2 (+) (Just 5) (Just 6)
{-
The starship operator applies a function that requires one value within an Applicative Functor
into another Applicative context that has the value it needs
-}
asApplicativeLiftA2' = (Just (+5)) <*>(Just 6)
  
just_11 :: Maybe Int
just_11 = liftA2 (+) (Just 5) (Just 6)

v1 = [(Just (+5))]
v2 :: [ (Maybe Int ) ]
v2 = [(Just 6)] 
v3 :: [Maybe Int]
v3 = liftA2 (<*>) v1 v2
just_9 :: [Maybe Int]
just_9 = liftA2 (<*>) [(Just (+5))] [(Just 4)]

-- liftM2 :: Monad m => (a -> b -> c) 
-- zipWith ::  (a -> b -> c) -> [a] -> [b] -> [c]
liftA2', liftM2', zipWith' :: [Int]
liftA2' = liftM2 (+) [1,2,3][4,5,6]
liftM2' = liftA2 (+) [1,2,3][4,5,6]
zipWith' = zipWith (+) [1,2,3][4,5,6]

bindingAndSequencing :: IO ()
bindingAndSequencing  = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn $ "Hello " ++ name ++ " !!!"

bindingAndSequencing' :: IO ()
bindingAndSequencing' =  
  putStrLn "Enter your name: " >>
  getLine >>=
  (\name ->  putStrLn $ "Hello " ++ name ++ " !!!" )

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

nonEmpty :: String -> Maybe String
nonEmpty "" = Nothing
nonEmpty s = Just s
  
nonNegative :: Int -> Maybe Int
nonNegative  n | n >= 0 = Just n
               | otherwise = Nothing
               
-- make sure Bess the cow is under 500
weightCheck :: Cow -> Maybe Cow
weightCheck cow =
  let _name = name cow
      _weight = weight cow
  in if _name == "Bess" && _weight >= 500
        then Nothing
        else Just cow

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow  name' age' weight' =
  case nonEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case nonNegative age' of
        Nothing -> Nothing
        Just agy ->
          case nonNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agy weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'  name' age' weight' = do
  name'' <- nonEmpty name'
  age'' <- nonNegative age'
  weight'' <- nonNegative weight'
  weightCheck (Cow name'' age'' weight'')

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow''  name' age' weight' =
  (nonEmpty name') >>= (\n ->
                          (nonNegative age') >>= (\a ->
                                                    (nonNegative weight') >>= (\w ->
                                                                                (weightCheck (Cow n a w)))))
