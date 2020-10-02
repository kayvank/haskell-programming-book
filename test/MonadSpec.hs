-- |

module MonadSpec where
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Control.Exception              ( evaluate )
import           QSort
import           CountMe

instance Eq a => EqProp (CountMe a)  where
  (=-=) = eq
instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

spec :: Spec
spec = do

  it " sorting a sort list  idempotent" $ property $ \xs ->
    qsort (qsort xs) == qsort (xs :: [Int])

  -- let trigger :: CountMe (Int, String, String)
  --     trigger = undefined
  -- quickBatch $ functor trigger
