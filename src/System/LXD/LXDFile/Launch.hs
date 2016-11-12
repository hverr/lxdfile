{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Launch LXD images with init scripts
module System.LXD.LXDFile.Launch (
  launch
) where

import Prelude hiding (writeFile)

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Filesystem.Path.CurrentOS (decodeString)
import Turtle (echo, rm)

import Language.LXDFile.InitScript (InitScript(..))
import System.LXD.LXDFile.ScriptAction (HasContext(..), scriptActions, runScriptAction, tmpfile)
import System.LXD.LXDFile.Utils.Shell (HasContainer(..), lxc, lxcExec, lxcFilePush)

data LaunchCtx = LaunchCtx { initScripts :: [InitScript]
                           , image :: Text
                           , container :: Text
                           , context :: FilePath }

launch :: (MonadIO m, MonadError String m) => String -> String -> FilePath -> [InitScript] -> m ()
launch image' container' ctx' scripts' =
    let ctx = LaunchCtx scripts' (pack image') (pack container') ctx' in
    flip runReaderT ctx $ do
        launchContainer
        ask >>= mapM_ runInitScript . initScripts
        includeInitScripts
        echo $ "Successfully initialized " <> pack container'

launchContainer :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
launchContainer = do
    i <- image <$> ask
    c <- container <$> ask
    echo $ "Launching " <> i <> " as " <> c
    lxc ["launch", i, c]

runInitScript :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => InitScript -> m ()
runInitScript s = mapM_ runScriptAction $ scriptActions (actions s)

includeInitScripts :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
includeInitScripts = do
    file <- tmpfile "lxdfile-metadata-initscripts"
    ask >>= liftIO . writeFile file . encodePretty . initScripts
    lxcExec ["mkdir", "-p", "/etc/lxdfile"]
    lxcFilePush "0644" file "/etc/lxdfile/initscripts"
    rm (decodeString file)

instance MonadReader LaunchCtx m => HasContainer m where
    askContainer = container <$> ask

instance MonadReader LaunchCtx m => HasContext m where
    askContext = context <$> ask
