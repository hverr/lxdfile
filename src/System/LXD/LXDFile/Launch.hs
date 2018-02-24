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

import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Coerce (coerce)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Filesystem.Path.CurrentOS (decodeString)
import Turtle (rm, sleep)

import Network.LXD.Client.Commands
    (HasClient(..),
     ContainerCreateRequest(..), containerCreateRequest,
     ContainerName(..),
     lxcCreate, lxcStart, lxcFileMkdir, lxcFilePush)

import qualified System.LXD.Client.Config as LXC


import System.LXD.LXDFile.Inject (InitScriptContext(..), runInitScript)
import System.LXD.LXDFile.ScriptAction (tmpfile)
import System.LXD.LXDFile.Types (Image, containerSourceFromImage)
import System.LXD.LXDFile.Utils.Line (echoS, echoT)
import System.LXD.LXDFile.Utils.Text (showT)

type Profile = Maybe String

data LaunchCtx = LaunchCtx { lxcConfig :: LXC.Config
                           , initScripts :: [InitScriptContext]
                           , image :: Image
                           , profile :: Profile
                           , container :: ContainerName }

launch :: HasClient m => LXC.Config -> Image -> ContainerName -> Profile -> [InitScriptContext] -> m ()
launch lxcCfg image' container' profile' scripts' =
    let ctx = LaunchCtx lxcCfg scripts' image' profile' container' in
    flip runReaderT ctx $ do
        c <- container <$> ask
        launchContainer
        sleep 4.0
        ask >>= mapM_ (lift . flip runReaderT c . runInitScript) . initScripts
        includeInitScripts
        echoS $ "Successfully initialized " <> coerce container'

launchContainer :: HasClient m => ReaderT LaunchCtx m ()
launchContainer = do
    i <- image <$> ask
    c <- container <$> ask
    p <- profile <$> ask
    cfg <- lxcConfig <$> ask
    echoT $ "Launching " <> showT i <> " as " <> T.pack (coerce c)

    let p' = case p of Nothing -> []
                       Just x  -> [x]

    img <- case containerSourceFromImage cfg i of
        Left e -> throwM $ userError e
        Right x -> return x
    let req' = containerCreateRequest (coerce c) img
        req = req' { containerCreateRequestProfiles = p' }
    lift $ lxcCreate req
    lift $ lxcStart c


includeInitScripts :: HasClient m => ReaderT LaunchCtx m ()
includeInitScripts = do
    c <- container <$> ask
    file <- tmpfile "lxdfile-metadata-initscripts"
    ask >>= liftIO . writeFile file . encodePretty . map initScript . initScripts
    lift $ lxcFileMkdir c "/etc/lxdfile" True
    lift $ lxcFilePush c file "/etc/lxdfile/initscripts"

    rm (decodeString file)
