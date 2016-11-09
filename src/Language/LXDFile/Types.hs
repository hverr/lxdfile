{-# LANGUAGE FlexibleContexts #-}
module Language.LXDFile.Types where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Maybe (mapMaybe)

import Text.Parsec (ParseError)

data LXDFile = LXDFile { baseImage :: Image
                       , actions :: [Action] }
                       deriving (Show)

lxdFile :: MonadError ASTError m => AST -> m LXDFile
lxdFile ast = LXDFile <$> onlyOne NoBaseImage ManyBaseImages baseImages
                      <*> allActions
  where
    instructions = map instruction ast

    baseImages = mapMaybe baseImage' instructions
    baseImage' (From x) = Just x
    baseImage' _        = Nothing

    allActions = pure $ mapMaybe action' instructions
    action' (Action x) = Just x
    action' _          = Nothing

    onlyOne _ _       [x] = return x
    onlyOne noneErr _ []  = throwError noneErr
    onlyOne _ manyErr _   = throwError manyErr


data ASTError =
      ManyBaseImages
    | NoBaseImage

instance Show ASTError where
  show ManyBaseImages = "multiple base images specified"
  show NoBaseImage = "no base image specified"

data LXDFileError = ParseError ParseError | ASTError ASTError

instance Show LXDFileError where
    show (ParseError x) = show x
    show (ASTError x) = show x

type AST = [InstructionPos]

data InstructionPos = InstructionPos Instruction String Int deriving (Show)

data Instruction =
      Action Action
    | Comment String
    | From Image
    | EOL
    deriving (Show)

data Action =
      ChangeDirectory String
    | Copy Source Destination
    | Run Arguments
    deriving (Show)

type Destination = String
type Image = String
type Source = String

data Arguments = ArgumentsList [String]
               | ArgumentsShell String
               deriving (Show)

instruction :: InstructionPos -> Instruction
instruction (InstructionPos i _ _) = i
