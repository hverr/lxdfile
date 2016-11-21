{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module System.LXD.LXDFile.Inject (
  InitScriptContext(..)
, inject
, runInitScript
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, runReaderT)

import Data.Text (pack)

import Turtle (echo)

import Language.LXDFile.InitScript (InitScript(..))
import System.LXD.LXDFile.ScriptAction (scriptActions, runScriptAction)
import System.LXD.LXDFile.Utils.Shell (Container)

data InitScriptContext = InitScriptContext { initScript :: InitScript
                                           , context :: FilePath }

inject :: (MonadIO m, MonadError String m) => String -> [InitScriptContext] -> m ()
inject container' scripts' = do
    mapM_ (flip runReaderT container . runInitScript) scripts'
    echo "Successfully injected scripts"
  where
    container = pack container'

runInitScript :: (MonadIO m, MonadError String m, MonadReader Container m) => InitScriptContext -> m ()
runInitScript s = mapM_ run' actions'
  where
    actions' = scriptActions . actions $ initScript s
    run' = runScriptAction (context s)
