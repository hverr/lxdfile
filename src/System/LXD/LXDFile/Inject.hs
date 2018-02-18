{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module System.LXD.LXDFile.Inject (
  InitScriptContext(..)
, inject
, runInitScript
) where

import Control.Monad.Reader (ReaderT, runReaderT)

import Network.LXD.Client.Commands (HasClient(..), ContainerName(..))

import Turtle (echo)

import Language.LXDFile.InitScript (InitScript(..))
import System.LXD.LXDFile.ScriptAction (scriptActions, runScriptAction)

data InitScriptContext = InitScriptContext { initScript :: InitScript
                                           , context :: FilePath }

inject :: HasClient m => ContainerName -> [InitScriptContext] -> m ()
inject container scripts' = do
    mapM_ (flip runReaderT container . runInitScript) scripts'
    echo "Successfully injected scripts"

runInitScript :: HasClient m => InitScriptContext -> ReaderT ContainerName m ()
runInitScript s = mapM_ run' actions'
  where
    actions' = scriptActions . actions $ initScript s
    run' = runScriptAction (context s)
