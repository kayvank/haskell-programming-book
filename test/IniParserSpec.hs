{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module IniParserSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Data.ByteString                ( ByteString )
import qualified Text.Trifecta                 as T
import           Text.RawString.QQ
import qualified Data.Map                      as M

import           IniParser

maybeSuccess :: T.Result a -> Maybe a
maybeSuccess (T.Success a) = Just a
maybeSuccess _             = Nothing

sectionEx :: ByteString
sectionEx = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx' :: ByteString
sectionEx' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

commentEx :: ByteString
commentEx = "; last modified 1 April\ \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n;hah"

spec :: Spec
spec = do
  describe "Ini parser specs are:" $ do
    it "can parse simple assignments" $ do
      let asignmentEx = "woot=1" :: ByteString
          m           = T.parseByteString parserAssignment mempty asignmentEx
          r'          = maybeSuccess m
      r' `shouldBe` Just ("woot", "1")
    it "can parse simple header" $ do
      let headerEx = "[blah]" :: ByteString
          m        = T.parseByteString parserHeader mempty headerEx
          r'       = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
    it "skips comments before header" $ do
      let
        headerWithComment = "; skip this comment\n[blah]" :: ByteString
        m                 = T.parseByteString (skipComments >> parserHeader)
                                              mempty
                                              headerWithComment
        r' = maybeSuccess m
      r' `shouldBe` Just (Header "blah")

    it "can parse a simple section" $ do
      let m  = T.parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just
        (Section (Header "states") (M.singleton "Chris" "Texas"))

    it "can parse multiple sections" $ do
      let
        m  = T.parseByteString parseIni mempty sectionEx'
        r' = maybeSuccess m
        sV = M.singleton
          (Header "section")
          ((M.singleton "alias" "claw") <> (M.singleton "host" "wikipedia.org"))
        wV =
          M.singleton (Header "whatisit") (M.singleton "red" "intoothandclaw")
      print m
      r' `shouldBe` Just (Config (sV <> wV))
