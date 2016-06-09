{-# LANGUAGE QuasiQuotes #-}
module Jarvis.Hint.UnconditionalIfStatementSpec where

import Test.Hspec
import Text.RawString.QQ

import Control.Monad (forM_)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))

import Jarvis.Hint.UnconditionalIfStatement
import Jarvis.Idea
import Jarvis.Hint.Helper

srcBad1 :: String
srcBad1 = [r|
class Foo {
  public void close() {
    if (true) {
    }
  }
}
|]

srcBad2 :: String
srcBad2 = [r|
class Foo {
  public void close() {
    if (false) {
    }
  }
}
|]

srcBad3 :: String
srcBad3 = [r|
class Foo {
  public void close() {
    if (true) {
    } else {
    }
  }
}
|]

srcBad4 :: String
srcBad4 = [r|
class Foo {
  public void close() {
    if (false) {
    } else {
    }
  }
}
|]

srcGood :: String
srcGood = [r|
class Foo {
  public void close(bool flag) {
    if (flag) {
    }
  }
}
|]

isUnconditionalIfStatementHint :: Idea -> Bool
isUnconditionalIfStatementHint Idea{ideaHint=ideaHint} =
  ideaHint == "Do not use 'if' statements that are always true or always false"

spec :: Spec
spec = do
  describe "unconditionalIfStatementHint" $ do
    it "suggests refactoring when 'if' is always true or always false" $
      forM_ [srcBad1, srcBad2, srcBad3, srcBad4] (\src ->
        runHint unconditionalIfStatementHint src `shouldSatisfy` (all isUnconditionalIfStatementHint) .&&. (not . null))
    it "detects nothing otherwise" $
      runHint unconditionalIfStatementHint srcGood `shouldBe` []

