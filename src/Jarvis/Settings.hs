module Jarvis.Settings
  ( Severity(..)
  ) where

-- | How severe an issue is.
data Severity
    = Suggestion -- ^ Suggestions are things that some people may consider improvements, but some may not.
    | Warning -- ^ Warnings are suggestions that are nearly always a good idea to apply.
      deriving (Eq, Ord, Show, Read, Bounded, Enum)

