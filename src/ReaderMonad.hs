-- |

module ReaderMonad where

import           Data.Functor.Identity
import           Data.Char                     as C


data Reader cfg a = Reader { runReader :: cfg -> a }

data ABConfig = ABConfig
  {
    dont'UseLetterE :: Bool
  , dont'UseLetterL :: Bool
  }

toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str = filter processFilters (C.toUpper <$> str)
 where
  filters :: [Char -> Bool]
  filters =
    [ if dont'UseLetterE cfg then (/= 'E') else const True
    , if dont'UseLetterL cfg then (/= 'L') else const True
    ]
  processFilters :: Char -> Bool
  processFilters c = all (\f -> f c) filters

welcomeMessage :: ABConfig -> String -> String -> String
welcomeMessage cfg motd username =
  "Welcome"
    ++ toUpperStr cfg username
    ++ "! Message of the day: "
    ++ toUpperStr cfg motd

{-
aFunctionWithConfig :: config -> a
data Reader cfg a = Reader { runReader :: cfg -> a }
return :: Monad m => a -> m a
(>>=) :: Monad m => m a -> (a -> m b) -> m b
-}

toUpperStr' :: String -> Reader ABConfig String
toUpperStr' str = Reader
  (\cfg ->
    let filters :: [Char -> Bool]
        filters =
            [ if dont'UseLetterE cfg then (/= 'E') else const True
            , if dont'UseLetterL cfg then (/= 'L') else const True
            ]
        processFilters :: Char -> Bool
        processFilters c = all (\f -> f c) filters
    in  filter processFilters (C.toUpper <$> str)
  )

welcomeMessage' :: String -> String -> Reader ABConfig String
welcomeMessage' motd username =
  toUpperStr' motd
    >>= (\upperMOTD ->
          toUpperStr' username
            >>= (\upperUsername -> Reader
                  (\_ ->
                    "Welcome, "
                      ++ upperUsername
                      ++ "! Message of the the day:  "
                      ++ upperMOTD
                  )
                )
        )

instance Functor (Reader cfg) where
  -- fmap :: a -> b -> f a -> f b
  fmap f r = Reader (f . (runReader r))

instance Applicative (Reader cfg) where
  pure x = Reader (const x)
  -- (<*>) :: f (a -> b) -> f a -> fb
  --Reader cfgF <*> Reader cfgX = Reader (\cfg -> (cfgF cfg) (cfgX cfg))
  Reader cfgF <*> Reader cfgX = Reader
    (\cfg ->
      let f = cfgF cfg
          g = cfgX cfg
      in  f g
    )

instance Monad (Reader cfg) where
  return a = Reader (const a)
  -- (>>=) :: m a -> (a -> m b) -> m b
  Reader cfgMA >>= nextFN = Reader
    (\cfg ->
      let f = cfgMA cfg -- a
      in  runReader (nextFN f) cfg
    )

ask :: Reader cfg cfg
-- ask = Reader (\cfg -> cfg)
ask = Reader id

asks :: (cfg -> a) -> Reader cfg a
asks f = Reader (\cfg -> f cfg)

asks' :: (cfg -> a) -> Reader cfg a
asks' f = do
  cfg <- ask
  pure (f cfg)

toUpperStr'' :: String -> Reader ABConfig String
toUpperStr'' str = do
  cfg <- ask
  let filters :: [Char -> Bool]
      filters =
        [ if dont'UseLetterE cfg then (/= 'E') else const True
        , if dont'UseLetterL cfg then (/= 'L') else const True
        ]
  let processFilters :: Char -> Bool
      processFilters c = all (\f -> f c) filters
  pure (filter processFilters (C.toUpper <$> str))

welcomeMessage'' :: String -> String -> Reader ABConfig String
welcomeMessage'' motd username = do
  motd' <- toUpperStr'' motd
  uname <- toUpperStr'' username
  pure ("Welcome, " ++ uname ++ "! Message of the the day:  " ++ motd')
