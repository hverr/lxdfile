-- | This module contains funcitonality to parse InitScripts.
module Language.LXDFile.InitScript (
  -- * Parsing
  parseString
, parseFile

  -- * Top-level types
, InitScript(..)
, Action(..)

  -- * Subtypes
, Arguments(..)
) where

import Language.LXDFile.InitScript.Parser (parseString, parseFile)
import Language.LXDFile.InitScript.Types
