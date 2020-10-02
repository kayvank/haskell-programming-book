module ApplicativeSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Control.Exception              ( evaluate )
import           QSort
import           Bull


instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance EqProp Bull where
  (=-=) = eq
prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

genList :: Arbitrary a => gen [a]
genList = undefined

genTuple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)


spec :: Spec
spec = do
  describe "specs are:" $ do
    it "returns the first emlement of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it " returns the first element of an *arbitrary* list"
      $ property
      $ \x xs -> head (x : xs) == (x :: Int)

    it " reverse reversing list is idempotent" $ property $ \xs ->
      reverse (reverse xs) == (xs :: [Int])

    it " sorting a sort list  idempotent" $ property $ \xs ->
      qsort (qsort xs) == qsort (xs :: [Int])

    it "throws an exception if used with empty list" $ do
      evaluate (head []) `shouldThrow` anyException
