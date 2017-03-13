{-# LANGUAGE GADTs #-}
module Commands.Types where

import System.LXD.LXDFile.Launch (Profile)

type BaseDirectory = FilePath
type BaseImage = String
type Container = String
type ContextDirectory = FilePath
type InitScript = FilePath
type ImageTag = String
type LXDFilePath = FilePath

data Command where
    Build :: LXDFilePath
           -> ImageTag
           -> BaseDirectory
           -> Maybe BaseImage
           -> Command
    Launch :: ImageTag
           -> Container
           -> Profile
           -> [InitScriptArgument]
           -> Command
    Inject :: Container
           -> [InitScriptArgument]
           -> Command
    Volume :: Volume
           -> Command
    Version :: Command

data Volume where
    List :: Container
         -> Volume

data InitScriptArgument where
    InitScriptArgument :: InitScript
                       -> ContextDirectory
                       -> InitScriptArgument
