module Jarvis.Hint.All
  ( builtin
  , builtinHints
  , HintBuiltin(..)
  ) where

import Jarvis.Hint.Type
import Jarvis.Hint.BadCovariantDefinitionOfEquals
import Jarvis.Hint.BadComparisonWithBoolean

data HintBuiltin =
    HintBadCovariantDefinitionOfEquals
    | HintBadComparisonWithBoolean
  deriving (Show,Eq,Ord,Bounded,Enum)

builtin :: HintBuiltin -> Hint
builtin x = case x of
  HintBadCovariantDefinitionOfEquals -> typeDecl badCovariantDefinitionOfEqualsHint
  HintBadComparisonWithBoolean -> typeDecl badComparisonWithBooleanHint
  where
    typeDecl x = mempty{hintTypeDecl=x}

builtinHints :: [(String, Hint)]
builtinHints = [(show h, builtin h) | h <- [minBound .. maxBound]]

