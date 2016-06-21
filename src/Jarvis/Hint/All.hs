module Jarvis.Hint.All
  ( builtin
  , builtinHints
  , HintBuiltin(..)
  ) where

import Jarvis.Hint.Type
import Jarvis.Hint.BadCovariantDefinitionOfEquals
import Jarvis.Hint.BadComparisonWithBoolean
import Jarvis.Hint.ReturnInBothTryAndFinally
import Jarvis.Hint.UnconditionalIfStatement

data HintBuiltin =
    HintBadCovariantDefinitionOfEquals
  | HintBadComparisonWithBoolean
  | HintReturnInBothTryAndFinally
  | HintUnconditionalIfStatement
  deriving (Show,Eq,Ord,Bounded,Enum)

builtin :: HintBuiltin -> Hint
builtin x = case x of
  HintBadCovariantDefinitionOfEquals -> typeDecl badCovariantDefinitionOfEqualsHint
  HintBadComparisonWithBoolean -> typeDecl badComparisonWithBooleanHint
  HintReturnInBothTryAndFinally -> typeDecl returnInBothTryAndFinallyHint
  HintUnconditionalIfStatement -> typeDecl unconditionalIfStatementHint
  where
    typeDecl x = mempty{hintTypeDecl=x}

builtinHints :: [(String, Hint)]
builtinHints = [(show h, builtin h) | h <- [minBound .. maxBound]]

