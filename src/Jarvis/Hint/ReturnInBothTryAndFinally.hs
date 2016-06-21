module Jarvis.Hint.ReturnInBothTryAndFinally
  ( returnInBothTryAndFinallyHint
  ) where

import Data.Generics.Uniplate.Data
import Language.Java.Syntax

import Jarvis.Hint.Type
import Jarvis.Idea

returnInBothTryAndFinallyHint :: TypeDeclHint
returnInBothTryAndFinallyHint typeDecl =
  [
    warn "Do not use 'return' statement in both try and finally statement" t t |
      t@(Try tryBlock _ (Just finallyBlock)) <- universeBi typeDecl,
      (Return _) <- universeBi tryBlock,
      (Return _) <- universeBi finallyBlock
  ]
