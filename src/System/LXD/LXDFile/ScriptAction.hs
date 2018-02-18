{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.LXD.LXDFile.ScriptAction where

import Control.Lens (Lens', lens, (^.), (.~), (%~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (State, evalState, modify, get)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)

import Data.Foldable (foldlM)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BL

import Filesystem.Path.CurrentOS (decodeString)
import Turtle (Line, echo, output, rm, select)
import qualified Codec.Archive.Tar as Tar

import Network.LXD.Client.Commands
    (HasClient(..), ContainerName(..),
     lxcFileMkdir, lxcFilePush, lxcExec)

import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import System.FilePath (takeDirectory)

import Language.LXDFile.Types (Action(..), Arguments(..), Source, Destination, Key, Value)
import System.LXD.LXDFile.Utils.Line (showL, echoS, unsafeStringToLine)
import System.LXD.LXDFile.Utils.String (replace)

data ScriptCtx = ScriptCtx { _currentDirectory :: FilePath
                           , _environment :: [(String, String)] }

currentDirectory :: Lens' ScriptCtx FilePath
currentDirectory = lens _currentDirectory $ \s x -> s { _currentDirectory = x }

environment :: Lens' ScriptCtx [(String, String)]
environment = lens _environment $ \s x -> s { _environment = x }

defaultScriptCtx :: ScriptCtx
defaultScriptCtx = ScriptCtx "/" []

data ScriptAction = SRun ScriptCtx Arguments
                  | SCopy ScriptCtx Source Destination
                  | SChangeDirectory FilePath
                  | SEnvironment Key Value

scriptActions :: [Action] -> [ScriptAction]
scriptActions actions = reverse $ evalState (foldlM f [] actions) defaultScriptCtx
  where f xs a = (: xs) <$> scriptAction a

scriptAction :: Action -> State ScriptCtx ScriptAction
scriptAction (ChangeDirectory fp) = do
    modify $ \s -> (currentDirectory .~ fp) s
    return $ SChangeDirectory fp
scriptAction (Copy src dst) = SCopy <$> get <*> pure src <*> pure dst
scriptAction (Environment key value) = do
    modify $ \s -> (environment %~ \e -> e ++ [(key, value)]) s
    return $ SEnvironment key value
scriptAction (Run x) = SRun <$> get <*> pure x

currentDirectorySh :: ScriptCtx -> [Line]
currentDirectorySh ctx = ["cd " <> unsafeStringToLine (ctx ^. currentDirectory)]

environmentSh :: ScriptCtx -> [Line]
environmentSh ctx = map conv $ ctx ^. environment
   where conv (key, value) = unsafeStringToLine $ key ++ "=" ++ value

copyDest :: ScriptCtx -> Destination -> Destination
copyDest _ d@('/':_) = d
copyDest ctx dir = (ctx ^. currentDirectory) <> "/" <> dir

argumentsSh :: Arguments -> [Line]
argumentsSh (ArgumentsShell s) = [unsafeStringToLine s]
argumentsSh (ArgumentsList xs) = [unsafeStringToLine . unwords $ map escapeArg xs]
  where
    escapeArg = replace " " "\\ " . replace "\t" "\\\t" . replace "\"" "\\\"" . replace "'" "\\'"

runScriptAction :: HasClient m => FilePath -> ScriptAction -> ReaderT ContainerName m ()
runScriptAction _ (SRun ctx cmd) = do
    c <- ask
    echo $ "RUN " <> showL cmd
    script <- makeScript
    lift $ lxcFileMkdir c "/var/run/lxdfile" True
    lift $ lxcFilePush c script "/var/run/lxdfile/setup"
    lift $ lxcExec c "chmod" ["0700", "/var/run/lxdfile/setup"] "" >>= liftIO . BL.putStr
    rm (decodeString script)

    lift $ lxcExec c "/bin/sh" ["/var/run/lxdfile/setup"] "" >>= liftIO . BL.putStr
    lift $ lxcExec c "rm" ["-rf", "/var/run/lxdfile"] "" >>= liftIO . BL.putStr
  where
    makeScript = do
        fp <- tmpfile "lxdfile-setup.sh"
        let cmds' = [ "#!/bin/sh"
                    , "set -e"
                    ] ++ currentDirectorySh ctx
                      ++ environmentSh ctx ++ [
                      "set -x"
                    ] ++ argumentsSh cmd
        output (decodeString fp) $ select cmds'
        return fp

runScriptAction ctxDir (SCopy ctx src dst') = do
    c <- ask
    echoS $ "COPY " ++ src ++  " " ++ dst'
    let dst = copyDest ctx dst'
    tar <- createTar
    lift $ lxcExec c "mkdir" ["-p", "/var/run/lxdfile"] "" >>= liftIO . BL.putStr
    lift $ lxcFilePush c tar "/var/run/lxdfile/archive.tar"
    rm (decodeString tar)
    lift $ lxcExec c "mkdir" ["-p", "/var/run/lxdfile/archive"] "" >>= liftIO . BL.putStr
    lift $ lxcExec c "tar" ["-xf", "/var/run/lxdfile/archive.tar", "-C", "/var/run/lxdfile/archive"] "" >>= liftIO . BL.putStr
    lift $ lxcExec c "mkdir" ["-p", takeDirectory dst] "" >>= liftIO . BL.putStr
    lift $ lxcExec c "cp" ["-R", "/var/run/lxdfile/archive/" <> src, dst] "" >>= liftIO . BL.putStr
    lift $ lxcExec c "rm" ["-rf", "/var/run/lxdfile"] "" >>= liftIO . BL.putStr
  where
    createTar = do
        fp <- tmpfile "lxdfile-archive.tar"
        liftIO $ Tar.create fp ctxDir [src]
        return fp

runScriptAction _ (SChangeDirectory fp) = echoS $ "CD " <> fp
runScriptAction _ (SEnvironment key value) = echoS $ "ENV " <> key <> "=" <> value

tmpfile :: MonadIO m => String -> m FilePath
tmpfile template = do
    (fp, handle) <- liftIO $ getTemporaryDirectory >>= flip openTempFile template
    liftIO $ hClose handle
    return fp
