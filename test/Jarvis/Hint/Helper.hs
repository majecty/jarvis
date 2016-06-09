module Jarvis.Hint.Helper where

import Language.Java.Syntax
import Language.Java.Parser

import Jarvis.Idea
import Jarvis.Hint.Type

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

runHint :: TypeDeclHint -> String -> [Idea]
runHint hint src =
  let (Right (CompilationUnit _ _ typeDecls)) = parser compilationUnit src
  in concatMap hint typeDecls
