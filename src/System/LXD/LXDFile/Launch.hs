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
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Filesystem.Path.CurrentOS (decodeString)
import Turtle (rm, sleep)

import System.LXD.LXDFile.Inject (InitScriptContext(..), runInitScript)
import System.LXD.LXDFile.ScriptAction (tmpfile)
import System.LXD.LXDFile.Utils.Line (echoS, echoT)
import System.LXD.LXDFile.Utils.Shell (Container, lxc, lxcExec, lxcFilePush)

type Profile = Maybe String

data LaunchCtx = LaunchCtx { initScripts :: [InitScriptContext]
                           , image :: Text
                           , profile :: Profile
                           , container :: Text }

launch :: (MonadIO m, MonadError String m) => String -> String -> Profile -> [InitScriptContext] -> m ()
launch image' container' profile' scripts' =
    let ctx = LaunchCtx scripts' (pack image') profile' (pack container') in
    flip runReaderT ctx $ do
        c <- container <$> ask
        launchContainer
        sleep 4.0
        ask >>= mapM_ (flip runReaderT c . runInitScript) . initScripts
        includeInitScripts
        echoS $ "Successfully initialized " <> container'

launchContainer :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
launchContainer = do
    i <- image <$> ask
    c <- container <$> ask
    p <- profile <$> ask
    echoT $ "Launching " <> i <> " as " <> c
    case p of Nothing -> lxc ["launch", i, c]
              Just p' -> lxc ["launch", i, c, "--profile", pack p']

includeInitScripts :: (MonadIO m, MonadError String m, MonadReader LaunchCtx m) => m ()
includeInitScripts = do
    c <- container <$> ask
    file <- tmpfile "lxdfile-metadata-initscripts"
    ask >>= liftIO . writeFile file . encodePretty . map initScript . initScripts
    run $ lxcExec ["mkdir", "-p", "/etc/lxdfile"]
    flip runReaderT c $ lxcFilePush "0644" file "/etc/lxdfile/initscripts"
    rm (decodeString file)

run :: MonadReader LaunchCtx m => ReaderT Container m a -> m a
run x = container <$> ask >>= runReaderT x
