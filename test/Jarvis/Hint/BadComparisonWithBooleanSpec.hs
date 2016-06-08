{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module Jarvis.Hint.BadComparisonWithBooleanSpec where

import Test.Hspec
import Text.RawString.QQ

import Jarvis.Hint.Helper
import Jarvis.Hint.BadComparisonWithBoolean
import Jarvis.Settings
import Jarvis.Idea

import Data.Maybe

import Language.Java.Syntax 

srcBad = [r|
class Foo {
  void foo(boolean b) {
    if (b == false) {
    }
    while (b == true) {
    }
    if (false == bar(b)) {
    }
  }
}
|]

assertTo Idea{..} to = ideaTo `shouldBe` (Just to)

spec :: Spec
spec = do
  describe "badComparisonWithBooleanHint" $ do
    it "suggests with boolean compare" $
      let ideas = runHint badComparisonWithBooleanHint srcBad
      in map (fromJust.ideaTo) ideas `shouldBe` ["!b", "b", "!bar(b)"]
          
