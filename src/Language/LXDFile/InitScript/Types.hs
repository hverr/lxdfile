{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Except (MonadError, throwError)

import Data.Maybe (fromMaybe, mapMaybe)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Text.Parsec (ParseError)

import Language.LXDFile.Types (Action(..), Arguments(..))

data InitScript = InitScript { onUpdate :: Bool
                             , actions :: [Action] }
                             deriving (Generic, Show)

instance FromJSON InitScript where
instance ToJSON InitScript where

data InitScriptError = ParseError ParseError
                     | ASTError ASTError
                     deriving (Show)

initScript :: MonadError ASTError m => AST -> m InitScript
initScript ast = InitScript <$> (fromMaybe False <$> maybeOne ManyOnUpdates onUpdates)
                            <*> allActions
  where
    instructions = map instruction ast

    onUpdates = mapMaybe onUpdate' instructions
    onUpdate' (OnUpdate x) = Just x
    onUpdate' _            = Nothing

    allActions = pure $ mapMaybe action' instructions
    action' (Action x) = Just x
    action' _          = Nothing

    maybeOne _       [x] = return $ Just x
    maybeOne _       []  = return Nothing
    maybeOne manyErr _   = throwError manyErr

data ASTError =
      ManyOnUpdates

instance Show ASTError where
    show ManyOnUpdates = "multiple on update directives"

type AST = [InstructionPos]

data InstructionPos = InstructionPos Instruction String Int deriving (Show)

data Instruction =
      Action Action
    | Comment String
    | OnUpdate Bool
    | EOL
    deriving (Show)

instruction :: InstructionPos -> Instruction
instruction (InstructionPos i _ _) = i
