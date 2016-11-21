{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Launch LXD images with init scripts
module System.LXD.LXDFile.Launch (
  Profile
, InitScriptContext(..)
, launch
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
import Turtle (echo, rm, sleep)

import Language.LXDFile.InitScript (InitScript(..))
import System.LXD.LXDFile.ScriptAction (scriptActions, runScriptAction, tmpfile)
import System.LXD.LXDFile.Utils.Shell (HasContainer(..), lxc, lxcExec, lxcFilePush)

type Profile = Maybe String

data InitScriptContext = InitScriptContext { initScript :: InitScript
                                           , context :: FilePath }

data LaunchCtx = LaunchCtx { initScripts :: [InitScriptContext]
                           , image :: Text
                           , profile :: Profile
                           , container :: Text }

launch :: (MonadIO m, MonadError String m) => String -> String -> Profile -> [InitScriptContext] -> m ()
launch image' container' profile' scripts' =
    let ctx = LaunchCtx scripts' (pack image') profile' (pack container') in
    flip runReaderT ctx $ do
        launchContainer
        sleep 4.0
        ask >>= mapM_ runInitScript . initScripts
        includeInitScripts
        echo $ "Successfully initialized " <> pack container'

launchContainer :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
launchContainer = do
    i <- image <$> ask
    c <- container <$> ask
    p <- profile <$> ask
    echo $ "Launching " <> i <> " as " <> c
    case p of Nothing -> lxc ["launch", i, c]
              Just p' -> lxc ["launch", i, c, "--profile", pack p']

runInitScript :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => InitScriptContext -> m ()
runInitScript s = mapM_ run' actions'
  where
    actions' = scriptActions . actions $ initScript s
    run' = runScriptAction (context s)

includeInitScripts :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
includeInitScripts = do
    file <- tmpfile "lxdfile-metadata-initscripts"
    ask >>= liftIO . writeFile file . encodePretty . map initScript . initScripts
    lxcExec ["mkdir", "-p", "/etc/lxdfile"]
    lxcFilePush "0644" file "/etc/lxdfile/initscripts"
    rm (decodeString file)

instance MonadReader LaunchCtx m => HasContainer m where
    askContainer = container <$> ask
