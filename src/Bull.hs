-- | 

module Bull where

data Bull = Fools | Twoo deriving (Eq, Show)

instance Semigroup Bull where
  -- (<>) :: Bull -> Bul -> Bull
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
