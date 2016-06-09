{-# LANGUAGE QuasiQuotes #-}
module Jarvis.Hint.BadCovariantDefinitionOfEqualsSpec where

import Test.Hspec
import Text.RawString.QQ

import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))

import Jarvis.Hint.BadCovariantDefinitionOfEquals
import Jarvis.Idea
import Jarvis.Hint.Helper

srcBad :: String
srcBad = [r|
class Foo {
  public boolean equals(Foo other) {
    return true;
  }
}
|]

srcGood :: String
srcGood = [r|
class Foo {
  @Override
  public boolean equals(Object other) {
    return true;
  }
}
|]

isBadCovariantDefinitionOfEqualsHint :: Idea -> Bool
isBadCovariantDefinitionOfEqualsHint Idea{ideaHint=ideaHint} =
  ideaHint == "Bad covariant definition of equals"

spec :: Spec
spec = do
  describe "badCovariantDefinitionOfEqualsHint" $ do
    it "warns with bad definition of equals" $
      runHint badCovariantDefinitionOfEqualsHint srcBad `shouldSatisfy` (all isBadCovariantDefinitionOfEqualsHint) .&&. (not . null)
    it "detects nothing with good definition of equals" $
      runHint badCovariantDefinitionOfEqualsHint srcGood `shouldBe` []

