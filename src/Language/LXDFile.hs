-- | This module contains functionality to parse LXDFiles.
module Language.LXDFile (
  -- * Parsing
  parseString
, parseFile

  -- * Top-level types
, LXDFile(..)
, Action(..)
, Image

  -- * Subtypes
, Arguments(..)
, Destination
, Source
) where

import Language.LXDFile.Parser (parseString, parseFile)
import Language.LXDFile.Types
