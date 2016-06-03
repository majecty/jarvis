{-# LANGUAGE RecordWildCards #-}

module Jarvis.Idea
  ( Idea(..)
  , idea
  , rawIdea
  , rawIdeaN
  , suggest
  , warn
  ) where

import Jarvis.Settings
import Language.Java.Pretty

-- | An idea suggest by a 'Jarvis'.
data Idea = Idea
  { ideaPackage :: String -- ^ The package the idea applies to.
  , ideaTypeDecl :: String -- ^ The type declaration the idea applies to, typically the class name, but may be an interface name.
  , ideaSeverity :: Severity -- ^ The severity of the idea, e.g. 'Warning'.
  , ideaHint :: String -- ^ The name of the hint that generated the idea.
  , ideaFrom :: String -- ^ The contents of the source code the idea relates to.
  , ideaTo :: Maybe String -- ^ The suggested replacement, or 'Nothing' for no replacement (e.g. on parse errors).
  } deriving (Eq)

instance Show Idea where
  show = showEx id

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
  [(if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint)] ++
  f "Found" (Just ideaFrom) ++ f "Why not" ideaTo
  where
    f msg Nothing = []
    f msg (Just x) | null xs = [msg ++ " remove it."]
                   | otherwise = (msg ++ ":") : map ("  "++) xs
      where xs = lines $ tt x

rawIdea = Idea "" ""
rawIdeaN a b c d = Idea "" "" a b c d

idea :: (Pretty a, Pretty b) => Severity -> String -> a -> b -> Idea
idea severity hint from to = rawIdea severity hint (prettyPrint from) (Just $ prettyPrint to)

suggest :: (Pretty a, Pretty b) => String -> a -> b -> Idea
suggest a b c = idea Suggestion a b c

warn :: (Pretty a, Pretty b) => String -> a -> b -> Idea
warn a b c = idea Warning a b c
