-- | Module to interact with lxdfiles.
module System.LXD.LXDFile (
  module System.LXD.LXDFile.Volume
, build
, inject
, launch
) where

import System.LXD.LXDFile.Build (build)
import System.LXD.LXDFile.Inject (inject)
import System.LXD.LXDFile.Launch (launch)

import System.LXD.LXDFile.Volume
