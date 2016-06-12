module Jarvis.Hint.BadCovariantDefinitionOfEquals
  ( badCovariantDefinitionOfEqualsHint
  ) where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax

import Jarvis.Hint.JavaSyntaxHelper
import Jarvis.Hint.Type
import Jarvis.Idea

fixBadCovariantDefinitionOfEquals :: MemberDecl -> MemberDecl
fixBadCovariantDefinitionOfEquals = transform $ \x ->
  case x of
    MethodDecl a b c d [FormalParam x _ y z] e f -> MethodDecl a b c d [FormalParam x objectType y z] e f
    x -> x

badCovariantDefinitionOfEqualsHint :: TypeDeclHint
badCovariantDefinitionOfEqualsHint typeDecl =
  [warn "Bad covariant definition of equals" m (fixBadCovariantDefinitionOfEquals m) | m@(MethodDecl _ _ _ (Ident "equals") [FormalParam _ fpType _ _] _ _) <- universeBi typeDecl, fpType /= objectType]

