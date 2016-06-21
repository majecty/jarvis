{-# LANGUAGE QuasiQuotes #-}
module Jarvis.Hint.ReturnInBothTryAndFinallySpec where

import Test.Hspec
import Text.RawString.QQ

import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))

import Jarvis.Hint.ReturnInBothTryAndFinally
import Jarvis.Idea
import Jarvis.Hint.Helper

srcBad :: String
srcBad = [r|
class Foo {
  int foo() {
    try {
      return 0;
    } finally {
      return 1;
    }
  }
}
|]

srcGood :: String
srcGood = [r|
class Foo {
  int foo() {
    try {
      return 0;
    } finally {
    }
  }
}
|]

isReturnInBothTryAndFinally :: Idea -> Bool
isReturnInBothTryAndFinally Idea{ideaHint=ideaHint} =
  ideaHint == "Do not use 'return' statement in both try and finally statement"

spec :: Spec
spec = do
  describe "returnInBothTryAndFinally" $ do
    it "warns with using return statement in both try and final statement" $
      runHint returnInBothTryAndFinallyHint srcBad `shouldSatisfy` (all isReturnInBothTryAndFinally) .&&. (not . null)
    it "detects nothing with good try finally statements" $
      runHint returnInBothTryAndFinallyHint srcGood `shouldBe` []

