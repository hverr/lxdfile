{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LXDFile.Types where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Maybe (mapMaybe)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Text.Parsec (ParseError)

data LXDFile = LXDFile { baseImage :: Image
                       , description :: Maybe String
                       , volumes :: [FilePath]
                       , actions :: [Action] }
                       deriving (Generic, Show)

instance FromJSON LXDFile where
instance ToJSON LXDFile where

lxdFile :: MonadError ASTError m => AST -> m LXDFile
lxdFile ast = LXDFile <$> onlyOne NoBaseImage ManyBaseImages baseImages
                      <*> maybeOne ManyDescriptions descriptions
                      <*> allVolumes
                      <*> allActions
  where
    instructions = map instruction ast

    baseImages = mapMaybe baseImage' instructions
    baseImage' (From x) = Just x
    baseImage' _        = Nothing

    descriptions = mapMaybe description' instructions
    description' (Description x) = Just x
    description' _               = Nothing

    allActions = pure $ mapMaybe action' instructions
    action' (Action x) = Just x
    action' _          = Nothing

    allVolumes = pure $ mapMaybe volume' instructions
    volume' (Volume x) = Just x
    volume' _          = Nothing

    onlyOne _ _       [x] = return x
    onlyOne noneErr _ []  = throwError noneErr
    onlyOne _ manyErr _   = throwError manyErr

    maybeOne _       [x] = return $ Just x
    maybeOne _       []  = return Nothing
    maybeOne manyErr _   = throwError manyErr


data ASTError =
      ManyBaseImages
    | ManyDescriptions
    | NoBaseImage
    | NoDescription

instance Show ASTError where
  show ManyBaseImages = "multiple base images specified"
  show ManyDescriptions = "multiple descriptions specified"
  show NoBaseImage = "no base image specified"
  show NoDescription = "no description specified"

data LXDFileError = ParseError ParseError | ASTError ASTError

instance Show LXDFileError where
    show (ParseError x) = show x
    show (ASTError x) = show x

type AST = [InstructionPos]

data InstructionPos = InstructionPos Instruction String Int deriving (Show)

data Instruction =
      Action Action
    | Comment String
    | Description String
    | From Image
    | Volume FilePath
    | EOL
    deriving (Show)

data Action =
      ChangeDirectory String
    | Copy Source Destination
    | Environment Key Value
    | Run Arguments
    deriving (Generic, Show)

instance FromJSON Action where
instance ToJSON Action where

type Destination = String
type Image = String
type Source = String
type Key = String
type Value = String

data Arguments = ArgumentsList [String]
               | ArgumentsShell String
               deriving (Generic)

instance Show Arguments where
    show (ArgumentsList x) = show x
    show (ArgumentsShell x) = show x

instance FromJSON Arguments
instance ToJSON Arguments

instruction :: InstructionPos -> Instruction
instruction (InstructionPos i _ _) = i
