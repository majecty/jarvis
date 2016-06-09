module Jarvis.Hint.UnconditionalIfStatement
  ( unconditionalIfStatementHint
  ) where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax

import Jarvis.Hint.Type
import Jarvis.Idea

trueExp :: Exp
trueExp = Lit (Boolean True)

falseExp :: Exp
falseExp = Lit (Boolean False)

fixUnconditionalIfStatement :: Stmt -> Stmt
fixUnconditionalIfStatement (IfThen exp stmt) =
  if exp == trueExp
     then stmt
     else Empty
fixUnconditionalIfStatement (IfThenElse exp thenStmt elseStmt) =
  if exp == trueExp
     then thenStmt
     else elseStmt

unconditionalIfStatementHint :: TypeDeclHint
unconditionalIfStatementHint typeDecl =
  let stmts = [stmt | stmt@(IfThen exp _) <- universeBi typeDecl, exp == trueExp || exp == falseExp] ++ [stmt | stmt@(IfThenElse exp _ _) <- universeBi typeDecl, exp == trueExp || exp == falseExp]
      msg = "Do not use 'if' statements that are always true or always false"
   in [suggest msg stmt (fixUnconditionalIfStatement stmt) | stmt <- stmts]

