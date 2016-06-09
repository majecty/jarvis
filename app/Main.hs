module Main where

import Control.Monad
import Options.Applicative
import System.Exit

import Jarvis

opts = info (helper <*> cmdOption)
  ( fullDesc
  <> progDesc "jarvis"
  <> header "jarvis - gives suggestions on how to improve your Java code" )
  where
    cmdOption = JarvisOption
       <$> option (auto :: ReadM Int)
        ( long "java-version"
        <> value 8
        <> help "Java version [default to 8]")
       <*> switch
        ( long "version"
        <> short 'V'
        <> help "Show jarvis version" )
       <*> switch
        ( long "json"
        <> help "Display hint data as JSON" )
       <*> many (argument str (metavar "FILE"))

main :: IO ()
main = do
  jarvisOption <- execParser opts
  errs <- jarvis jarvisOption
  unless (null errs) $
      exitWith $ ExitFailure 1
