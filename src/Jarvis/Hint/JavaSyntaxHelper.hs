-- |
-- A collection of common Java types used by Jarvis hints
--
module Jarvis.Hint.JavaSyntaxHelper
  ( objectType
  ) where

import Language.Java.Syntax

objectType :: Type
objectType = RefType . ClassRefType . ClassType $ [(Ident "Object", [])]
