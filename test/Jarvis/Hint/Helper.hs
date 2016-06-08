module Jarvis.Hint.Helper where

import Language.Java.Syntax 
import Language.Java.Parser
  
import Jarvis.Idea
import Jarvis.Hint.Type

runHint :: TypeDeclHint -> String -> [Idea]
runHint hint src =
  let (Right (CompilationUnit _ _ typeDecls)) = parser compilationUnit src
  in concatMap hint typeDecls