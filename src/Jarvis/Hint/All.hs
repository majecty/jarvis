module Jarvis.Hint.All
  ( builtin
  , builtinHints
  , HintBuiltin(..)
  ) where

import Jarvis.Hint.Type
import Jarvis.Hint.BadCovariantDefinitionOfEquals

data HintBuiltin =
    HintBadCovariantDefinitionOfEquals
  deriving (Show,Eq,Ord,Bounded,Enum)

builtin :: HintBuiltin -> Hint
builtin x = case x of
  HintBadCovariantDefinitionOfEquals -> typeDecl badCovariantDefinitionOfEqualsHint
  where
    typeDecl x = mempty{hintTypeDecl=x}

builtinHints :: [(String, Hint)]
builtinHints = [(show h, builtin h) | h <- [minBound .. maxBound]]

