{-# LANGUAGE DeriveGeneric #-}
module Language.LXDFile.InitScript.Types (
  InitScript(..)
, initScript
, AST
, Instruction(..)
, InstructionPos(..)
, Action(..)
, Arguments(..)
, InitScriptError(..)
) where

import Data.Maybe (mapMaybe)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Text.Parsec (ParseError)

import Language.LXDFile.Types (Action(..), Arguments(..))

data InitScript = InitScript { actions :: [Action] }
                             deriving (Generic, Show)

instance FromJSON InitScript where
instance ToJSON InitScript where

data InitScriptError = ParseError ParseError
                     deriving (Show)

initScript :: Monad m => AST -> m InitScript
initScript ast = InitScript <$> allActions
  where
    instructions = map instruction ast

    allActions = pure $ mapMaybe action' instructions
    action' (Action x) = Just x
    action' _          = Nothing

type AST = [InstructionPos]

data InstructionPos = InstructionPos Instruction String Int deriving (Show)

data Instruction =
      Action Action
    | Comment String
    | EOL
    deriving (Show)

instruction :: InstructionPos -> Instruction
instruction (InstructionPos i _ _) = i
