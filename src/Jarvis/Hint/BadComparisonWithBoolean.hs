module Jarvis.Hint.BadComparisonWithBoolean 
  ( badComparisonWithBooleanHint
  ) where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax
import Jarvis.Hint.Type
import Jarvis.Idea (suggest, Idea)

fix (BinOp lhs op (Lit (Boolean bool))) = 
  if (op == Equal) == bool
  then lhs
  else (PreNot lhs) -- TODO if lhs is PreNot , normalize

fix (BinOp (Lit (Boolean bool)) op rhs) = 
  if (op == Equal) == bool
  then rhs
  else (PreNot rhs)

idea :: Exp -> Idea
idea e = suggest "Use boolean as it is or negate it" e (fix e)
 
badComparisonWithBooleanHint :: TypeDeclHint
badComparisonWithBooleanHint typeDecl = 
  [idea e| e@(BinOp _ op (Lit (Boolean _))) <- universeBi typeDecl, op == Equal || op == NotEq]
  ++ [idea e| e@(BinOp (Lit (Boolean _)) op _)<- universeBi typeDecl, op == Equal || op == NotEq]
  