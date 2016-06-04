{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Options.Generic
import System.Environment
import System.Exit

import Jarvis

main :: IO ()
main = do
  args <- getRecord "jarvis"
  errs <- jarvis (args :: [String])
  unless (null errs) $
      exitWith $ ExitFailure 1
