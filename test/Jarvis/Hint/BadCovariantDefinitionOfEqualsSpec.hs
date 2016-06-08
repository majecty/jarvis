{-# LANGUAGE QuasiQuotes #-}
module Jarvis.Hint.BadCovariantDefinitionOfEqualsSpec where

import Test.Hspec
import Text.RawString.QQ

import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))

import Jarvis.Hint.BadCovariantDefinitionOfEquals
import Jarvis.Idea

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

runHint :: String -> [Idea]
runHint src = let (Right (CompilationUnit _ _ typeDecls)) = parser compilationUnit src
              in concatMap badCovariantDefinitionOfEqualsHint typeDecls

spec :: Spec
spec = do
  describe "badCovariantDefinitionOfEqualsHint" $ do
    it "warns with bad definition of equals" $
      runHint srcBad `shouldSatisfy` (not . null)
    it "detects nothing with good definition of equals" $
      runHint srcGood `shouldBe` []

