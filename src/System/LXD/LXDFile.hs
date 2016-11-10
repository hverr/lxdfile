{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- | Module to interact with lxdfiles.
module System.LXD.LXDFile (
  build
) where

import Prelude hiding (writeFile)

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Control.Monad.State (State, evalState, get, put)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import Text.Parsec (parse, many, noneOf, string)

import Filesystem.Path.CurrentOS (decodeString)
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

data BuildAction = BuildRun (Maybe FilePath) [Arguments]
                 | BuildChangeDirectory FilePath
                 | BuildCopy Source Destination
                 deriving (Show)

data BuildCtx = BuildCtx { lxdfile :: LXDFile
                         , imageName :: String
                         , context :: FilePath
                         , buildContainer :: Text }

bundleActions :: [Action] -> [BuildAction]
bundleActions actions = reverse $ evalState (foldlM bundleActions' [] actions) Nothing
  where
    bundleActions' :: [BuildAction] -> Action -> State (Maybe FilePath) [BuildAction]
    bundleActions' xs (ChangeDirectory fp) = put (Just fp) >> return (BuildChangeDirectory fp : xs)
    bundleActions' xs (Copy src dst) = return $ BuildCopy src dst : xs
    bundleActions' (BuildRun wd args:xs) (Run x) = return $ BuildRun wd (args ++ [x]) : xs
    bundleActions' xs (Run x) = do r <- BuildRun <$> get <*> pure [x]
                                   return (r:xs)

build :: (MonadIO m, MonadError String m) => LXDFile -> String -> FilePath -> m ()
build lxdfile'@LXDFile{..} imageName' context' = do
    container <- launch `orThrowM` "error: could not launch container"
    let ctx = BuildCtx { lxdfile = lxdfile'
                       , imageName = imageName'
                       , context = context'
                       , buildContainer = container }
    flip runReaderT ctx $ do
        echo $ "Building " <> pack imageName' <> " in " <> container

        mapM_ buildAction $ bundleActions actions
        includeLXDFile

        echo $ "Stopping " <> container
        lxc ["stop", container]

        echo $ "Publishing to " <> pack imageName'
        case description of
            Nothing ->   lxc ["publish", container, format ("--alias=" % R.s) (pack imageName')]
            Just desc -> lxc ["publish", container, format ("--alias=" % R.s) (pack imageName'), format ("description=" % R.s) (pack desc)]
        lxc ["delete", container]
  where
    launch :: MonadIO m => m (Maybe Text)
    launch = fold (inproc "lxc" ["launch", pack baseImage] mempty) $
        Fold selectLaunchName Nothing id
    selectLaunchName (Just x) _ = Just x
    selectLaunchName _        x = parseLaunch x
    parseLaunch = (pack <$>) . rightToMaybe . parse (string "Creating " *> many (noneOf " ")) "" . unpack

buildAction :: (MonadIO m, MonadError String m, MonadReader BuildCtx m) => BuildAction -> m ()
buildAction (BuildRun wd cmds) = do
    script <- makeScript
    lxcExec ["mkdir", "/var/run/lxdfile"]
    lxcFilePush "0700" script "/var/run/lxdfile/setup"
    rm (decodeString script)
    lxcExec ["/var/run/lxdfile/setup"]
    lxcExec ["rm", "-rf", "/var/run/lxdfile"]
  where
    replace old new = intercalate new . splitOn old

    makeScript = do
        fp <- tmpfile "lxdfile-setup.sh"
        let cmds' = [ return "#!/bin/sh"
                    , return "set -ex"
                    ] ++ map ((cdToWd <>) . argumentsToShell) cmds
        output (decodeString fp) $ mconcat $ fmap (<> "\n") cmds'
        return fp

    cdToWd | Just wd' <- wd = return $ "cd " <> pack wd' <> "\n"
           | otherwise = mempty
    argumentsToShell (ArgumentsShell s) = return $ pack s
    argumentsToShell (ArgumentsList xs) = return . pack . unwords $ map escapeArg xs
    escapeArg = replace " " "\\ " . replace "\t" "\\\t" . replace "\"" "\\\"" . replace "'" "\\'"

buildAction (BuildCopy src dst) = do
    echo $ "COPY " <> pack src <> " " <> pack dst
    tar <- createTar
    lxcExec ["mkdir", "/var/run/lxdfile"]
    lxcFilePush "0600" tar "/var/run/lxdfile/archive.tar"
    rm (decodeString tar)
    lxcExec ["mkdir", "/var/run/lxdfile/archive"]
    lxcExec ["tar", "-xf", "/var/run/lxdfile/archive.tar", "-C", "/var/run/lxdfile/archive"]
    lxcExec ["mkdir", "-p", pack (takeDirectory dst)]
    lxcExec ["cp", "-R", "/var/run/lxdfile/archive/" <> pack src, pack dst]
    lxcExec ["rm", "-rf", "/var/run/lxdfile"]
  where
    createTar = do
        ctx <- context <$> ask
        fp <- tmpfile "lxdfile-archive.tar"
        liftIO $ Tar.create fp ctx [src]
        return fp

buildAction (BuildChangeDirectory _) = return ()

includeLXDFile :: (MonadIO m, MonadError String m, MonadReader BuildCtx m) => m ()
includeLXDFile = do
    file <- tmpfile "lxdfile-metadata-lxdfile"
    ask >>= liftIO . writeFile file . encodePretty . lxdfile
    lxcExec ["mkdir", "-p", "/etc/lxdfile"]
    lxcFilePush "0644" file "/etc/lxdfile/lxdfile"
    rm (decodeString file)

lxc :: (MonadIO m, MonadError String m) => [Text] -> m ()
lxc args = exec "lxc" args Nothing

lxcExec :: (MonadIO m, MonadError String m, MonadReader BuildCtx m) => [Text] -> m ()
lxcExec args = do
    c <- buildContainer <$> ask
    lxc $ ["exec", "--mode=non-interactive", c, "--"] ++ args

lxcFilePush :: (MonadIO m, MonadError String m, MonadReader BuildCtx m) => String -> FilePath -> FilePath -> m ()
lxcFilePush mode src dst = do
    c <- buildContainer <$> ask
    lxc ["file", "push", "--mode=" <> pack mode, pack src, c <> "/" <> pack dst]

tmpfile :: MonadIO m => String -> m FilePath
tmpfile template = do
    (fp, handle) <- liftIO $ getTemporaryDirectory >>= flip openTempFile template
    liftIO $ hClose handle
    return fp
