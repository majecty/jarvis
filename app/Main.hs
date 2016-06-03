module Main where

import Control.Monad
import System.Environment
import System.Exit

import Jarvis

main :: IO ()
main = do
  args <- getArgs
  errs <- jarvis args
  unless (null errs) $
      exitWith $ ExitFailure 1
