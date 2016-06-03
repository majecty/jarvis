module Jarvis.Hint.Type
  ( CompilationUnit
  , CrossHint
  , Hint(..)
  , TypeDeclHint
  ) where

import Data.Monoid
import Language.Java.Syntax

import Jarvis.Idea

type CrossHint = [CompilationUnit] -> [Idea]
type CompilationUnitHint = CompilationUnit -> [Idea]
type TypeDeclHint = TypeDecl -> [Idea]

-- | Functions to generate hints, combined using the 'Monoid' instance.
data Hint = Hint
  { hintCompilationUnits :: [CompilationUnit] -> [Idea] -- ^ Given a list of compilation units generate some 'Idea's.
  , hintCompilationUnit :: CompilationUnit -> [Idea] -- ^ Given a single compilation unit generate some 'Idea's.
  , hintTypeDecl :: TypeDecl -> [Idea] -- ^ Given a type decl generate some 'Idea's.
  }

instance Monoid Hint where
    mempty = Hint (const []) (const []) (const [])
    mappend (Hint x1 x2 x3) (Hint y1 y2 y3) =
        Hint (\a -> x1 a ++ y1 a) (\a -> x2 a ++ y2 a) (\a -> x3 a ++ y3 a)
