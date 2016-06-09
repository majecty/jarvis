{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Jarvis
  ( jarvis
  , JarvisOption(..)
  ) where

import Control.Monad.Extra
import Data.Foldable
import Data.Generics.Uniplate.Data
import Data.List (isSuffixOf)
import Data.Version (showVersion)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax (CompilationUnit(..))
import System.Directory
import System.FilePath

import Jarvis.Hint.All
import Jarvis.Hint.Type
import Jarvis.Idea
import Jarvis.Settings
import Paths_jarvis (version)

data JarvisOption = JarvisOption
  { javaVersion :: Int
  , versionFlag :: Bool
  , jsonFlag :: Bool
  , paths :: [FilePath]
  } deriving (Eq, Show)

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

getAllJavaPaths :: FilePath -> IO [FilePath]
getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

-- | This function takes a list of command line arguments, and returns the given hints.
jarvis :: JarvisOption -> IO [Idea]
jarvis option@(JarvisOption {..}) =
  if versionFlag
     then putStrLn ("jarvis " ++ showVersion version) >> return []
     else analyze option

analyze :: JarvisOption -> IO [Idea]
analyze (JarvisOption {..}) = do
  javaPaths <- concatMapM getAllJavaPaths paths
  r <- sequenceA <$> traverse parseJavaFile javaPaths
  case r of
    Left e -> do
      putStrLn (show e)
      return []
    Right compUnits -> do
      let ideas = applyCrossHint allHint compUnits ++ concatMap (applyHint allHint) compUnits
      if jsonFlag
         then putStrLn (showIdeasJson ideas)
         else traverse_ (putStrLn . show) ideas
      return ideas
  where
    allHint = fold $ fmap snd builtinHints
    parseJavaFile file = parser compilationUnit <$> readFile file

applyCrossHint :: Hint -> [CompilationUnit] -> [Idea]
applyCrossHint (Hint {..}) compUnits = hintCompilationUnits compUnits

applyHint :: Hint -> CompilationUnit -> [Idea]
applyHint (Hint {..}) cu@(CompilationUnit _ _ typeDecls) =
  hintCompilationUnit cu ++ concatMap hintTypeDecl typeDecls
