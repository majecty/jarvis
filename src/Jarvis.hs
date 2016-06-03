{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Jarvis
  ( jarvis
  ) where

import Data.Foldable
import Data.Generics.Uniplate.Data
import Data.List (isSuffixOf)
import Language.Java.Parser
import Language.Java.Syntax
import System.Directory
import System.FilePath

import Jarvis.Hint.All
import Jarvis.Hint.Type
import Jarvis.Idea
import Jarvis.Settings

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

getAllJavaPaths :: FilePath -> IO [FilePath]
getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

-- | This function takes a list of command line arguments, and returns the given hints.
jarvis :: [String] -> IO [Idea]
jarvis args = do
  javaPaths <- getAllJavaPaths (head args)
  r <- sequenceA <$> traverse parseJavaFile javaPaths
  case r of
    Left e -> do
      putStrLn (show e)
      return []
    Right compUnits -> do
      let ideas = applyCrossHint allHint compUnits ++ concatMap (applyHint allHint) compUnits
      traverse_ (putStrLn . show) ideas
      return ideas
  where
    allHint = fold $ fmap snd builtinHints
    parseJavaFile file = parser compilationUnit <$> readFile file

applyCrossHint :: Hint -> [CompilationUnit] -> [Idea]
applyCrossHint (Hint {..}) compUnits = hintCompilationUnits compUnits

applyHint :: Hint -> CompilationUnit -> [Idea]
applyHint (Hint {..}) cu@(CompilationUnit _ _ typeDecls) =
  hintCompilationUnit cu ++ concatMap hintTypeDecl typeDecls
