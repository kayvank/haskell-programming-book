-- |

module FoldableSpec where
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Control.Exception              ( evaluate )
import           Data.Monoid
import           Ch20
import           QSort


type PI = Product Int

spec :: Spec
spec = do

  describe "Chapter 20, Foldable specs are :" $ do

    it " sorting a sort list  idempotent" $ property $ \xs ->
      qsort (qsort xs) == qsort (xs :: [Int])

    it "foldMaps  indentity" $ do
      let fm = foldMap (* 5) (Identity 100) :: PI
      fm `shouldBe` Product (500)

    it "sum using foldMaps" $ do
      sum' [1 .. 10] `shouldBe` sum [1 .. 10]

    it "proudct using foldMaps" $ do
      prod' [1 .. 10] `shouldBe` product [1 .. 10]

    it "elem using foldMaps" $ do
      elem' 9 [1 .. 10] `shouldBe` True

    it "elem using foldMaps" $ do
      elem' 0 [1 .. 10] `shouldBe` False

    it "minimum using foldr" $ do
      minimum' [1 .. 10] `shouldBe` Just 1

    it "maxmum using foldr" $ do
      maximum' [1 .. 10] `shouldBe` Just 10

    it "length using foldl" $ do
      length' [1 .. 10] `shouldBe` length [1 .. 10]

    it "null  using foldl positive test" $ do
      null' [] `shouldBe` null []

    it "null  using foldl, negative test" $ do
      null' [1 .. 10] `shouldBe` null' [1 .. 10]
