{-
chapter 22 unit tests
-}

module ReaderPracticeSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Control.Exception              ( evaluate )
import           Data.Monoid
import           Ch22
import qualified ReaderPractice                as RP
import           QSort

spec :: Spec
spec = do

  describe "Chapter 22, ReaderPractice specs are :" $ do
    it " lookup should return the correct value" $ do
      (RP.lookup 3 $ zip RP.x RP.y) `shouldBe` Just 6

    it " x1 should be Just(6,9)" $ do
      RP.x1 `shouldBe` Just (6, 9)

    it " x2 should be Nothing" $ do
      RP.x2 `shouldBe` Nothing

    it " x3 should be (Just 9, Just 9)" $ do
      (RP.x3 3) `shouldBe` (Just 9, Just 9)

    it "summed pair" $ do
      (RP.summed (3, 4)) `shouldBe` 7

    it " bolt should for 3<x<8" $ do
      (RP.bolt 7) `shouldBe` True

    it " bolt should fail for out of range x in 3<x<8" $ do
      (RP.bolt 9) `shouldBe` False

    -- it " sequenceA [ list-1, list-2 ] == mconcat list-1, list-2" $ do
    --   (sequenceA [RP.x, RP.y]) `shouldBe` [RP.x ++ RP.y]
