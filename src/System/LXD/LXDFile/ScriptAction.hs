{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.ScriptAction where

import Control.Lens (Lens', lens, (^.), (.~), (%~))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (State, evalState, modify, get)
import Control.Monad.Reader (MonadReader)

import Data.Foldable (foldlM)
import Data.Monoid ((<>))
import Data.Text (pack)

import Filesystem.Path.CurrentOS (decodeString)
import Turtle (Line, echo, output, rm)
import qualified Codec.Archive.Tar as Tar

import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import System.FilePath (takeDirectory)

import Language.LXDFile.Types (Action(..), Arguments(..), Source, Destination, Key, Value)
import System.LXD.LXDFile.Utils.Line (showL, echoS, unsafeStringToLine)
import System.LXD.LXDFile.Utils.String (replace)
import System.LXD.LXDFile.Utils.Shell (Container, lxcExec, lxcFilePush)

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

runScriptAction :: (MonadIO m, MonadError String m, MonadReader Container m) => FilePath -> ScriptAction -> m ()
runScriptAction _ (SRun ctx cmd) = do
    echo $ "RUN " <> showL cmd
    script <- makeScript
    lxcExec ["mkdir", "-p", "/var/run/lxdfile"]
    lxcFilePush "0700" script "/var/run/lxdfile/setup"
    rm (decodeString script)
    lxcExec ["/bin/sh", "/var/run/lxdfile/setup"]
    lxcExec ["rm", "-rf", "/var/run/lxdfile"]
  where
    makeScript = do
        fp <- tmpfile "lxdfile-setup.sh"
        let cmds' = [ return "#!/bin/sh"
                    , return "set -e"
                    ] ++ map return (currentDirectorySh ctx)
                      ++ map return (environmentSh ctx) ++ [
                      return "set -x"
                    ] ++ map return (argumentsSh cmd)
        output (decodeString fp) $ mconcat cmds'
        return fp

runScriptAction ctxDir (SCopy ctx src dst') = do
    echoS $ "COPY " ++ src ++  " " ++ dst'
    let dst = copyDest ctx dst'
    tar <- createTar
    lxcExec ["mkdir", "-p", "/var/run/lxdfile"]
    lxcFilePush "0600" tar "/var/run/lxdfile/archive.tar"
    rm (decodeString tar)
    lxcExec ["mkdir", "-p", "/var/run/lxdfile/archive"]
    lxcExec ["tar", "-xf", "/var/run/lxdfile/archive.tar", "-C", "/var/run/lxdfile/archive"]
    lxcExec ["mkdir", "-p", pack (takeDirectory dst)]
    lxcExec ["cp", "-R", "/var/run/lxdfile/archive/" <> pack src, pack dst]
    lxcExec ["rm", "-rf", "/var/run/lxdfile"]
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
