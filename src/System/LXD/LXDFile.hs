{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- | Module to interact with lxdfiles.
module System.LXD.LXDFile (
 build
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Either.Combinators (rightToMaybe)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import Text.Parsec (parse, many, noneOf, string)

import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Turtle (Fold(..), fold, echo, output, inproc, rm, format, (%))
import qualified Codec.Archive.Tar as Tar
import qualified Turtle as R

import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import System.FilePath (takeDirectory)

import Language.LXDFile (LXDFile(..), Action(..), Arguments(..),
                         Destination, Source)
import System.LXD.LXDFile.Utils.Monad (orThrowM)
import System.LXD.LXDFile.Utils.Shell (exec)

data BuildAction = BuildRun [Arguments]
                 | BuildCopy Source Destination

bundleActions :: [Action] -> [BuildAction]
bundleActions = foldr bundleActions' []
  where
    bundleActions' (Copy src dst) xs= BuildCopy src dst:xs
    bundleActions' (Run x) (BuildRun args:xs) = BuildRun (args ++ [x]):xs
    bundleActions' (Run x) xs = BuildRun [x]:xs
build :: (MonadIO m, MonadError String m) => LXDFile -> String -> FilePath -> m ()
build LXDFile{..} name context = do
    container <- launch `orThrowM` "error: could not launch container"
    echo $ "Building " <> pack name <> " in " <> container
    mapM_ (buildAction container context) $ bundleActions actions
    echo $ "Stopping " <> container
    lxc ["stop", container]
    echo $ "Publishing to " <> pack name
    lxc ["publish", container, format ("--alias=" % R.s) (pack name)]
    lxc ["delete", container]
  where
    launch :: MonadIO m => m (Maybe Text)
    launch = fold (inproc "lxc" ["launch", pack baseImage] mempty) $
        Fold selectLaunchName Nothing id
    selectLaunchName (Just x) _ = Just x
    selectLaunchName _        x = parseLaunch x
    parseLaunch = (pack <$>) . rightToMaybe . parse (string "Creating " *> many (noneOf " ")) "" . unpack

buildAction :: (MonadIO m, MonadError String m) => Text -> FilePath -> BuildAction -> m ()
buildAction container _ (BuildRun cmds) = do
    script <- makeScript
    lxc ["exec", "--mode=non-interactive", container, "--", "mkdir", "/var/run/lxdfile"]
    lxc ["file", "push", "--mode=0700", toText script, container <> "/var/run/lxdfile/setup"]
    rm script
    lxc ["exec", "--mode=non-interactive", container, "--", "/var/run/lxdfile/setup"]
    lxc ["exec", "--mode=non-interactive", container, "--", "rm", "-rf", "/var/run/lxdfile"]
  where
    replace old new = intercalate new . splitOn old
    toText = pack . encodeString

    makeScript = do
        fp <- decodeString <$> tmpfile "lxdfile-setup.sh"
        let cmds' = map (return . pack . (++ "\n") . argumentsToShell) cmds
        output fp $ mconcat (return "#!/bin/sh\n\nset -ex\n\n" : cmds')
        return fp

    argumentsToShell (ArgumentsShell s) = s
    argumentsToShell (ArgumentsList xs) = unwords $ map escapeArg xs
    escapeArg = replace " " "\\ " . replace "\t" "\\\t" . replace "\"" "\\\"" . replace "'" "\\'"

buildAction container context (BuildCopy src dst) = do
    echo $ "COPY " <> pack src <> " " <> pack dst
    tar <- createTar
    lxc ["exec", "--mode=non-interactive", container, "--", "mkdir", "/var/run/lxdfile"]
    lxc ["file", "push", "--mode=0600", pack tar, container <> "/var/run/lxdfile/archive.tar"]
    rm (decodeString tar)
    lxc ["exec", "--mode=non-interactive", container, "--", "mkdir", "/var/run/lxdfile/archive"]
    lxc ["exec", "--mode=non-interactive", container, "--", "tar", "-xf", "/var/run/lxdfile/archive.tar", "-C", "/var/run/lxdfile/archive"]
    lxc ["exec", "--mode=non-interactive", container, "--", "mkdir", "-p", pack (takeDirectory dst)]
    lxc ["exec", "--mode=non-interactive", container, "--", "cp", "-R", "/var/run/lxdfile/archive/" <> pack src, pack dst]
    lxc ["exec", "--mode=non-interactive", container, "--", "rm", "-rf", "/var/run/lxdfile"]
  where
    createTar = do
        fp <- tmpfile "lxdfile-archive.tar"
        liftIO $ Tar.create fp context [src]
        return fp

lxc :: (MonadIO m, MonadError String m) => [Text] -> m ()
lxc args = exec "lxc" args Nothing

tmpfile :: MonadIO m => String -> m FilePath
tmpfile template = do
    (fp, handle) <- liftIO $ getTemporaryDirectory >>= flip openTempFile template
    liftIO $ hClose handle
    return fp
