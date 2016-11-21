{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Utils.Shell where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)

import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Turtle (ExitCode(..), proc)

type Container = Text

exec :: (MonadIO m, MonadError String m) => Text -> [Text] -> Maybe Text -> m ()
exec cmd args stdin = proc cmd args stdin' >>= toErr
  where
    stdin' | Just t <- stdin = return t
           | otherwise = mempty
    toErr ExitSuccess = return ()
    toErr (ExitFailure status) = throwError $ "error: exec " ++ show (cmd:args) ++ ": return code " ++ show status

lxc :: (MonadIO m, MonadError String m) => [Text] -> m ()
lxc args = exec "lxc" args Nothing

lxcExec :: (MonadIO m, MonadError String m, MonadReader Container m) => [Text] -> m ()
lxcExec args = do
    c <- ask
    lxc $ ["exec", "--mode=non-interactive", c, "--"] ++ args

lxcFilePush :: (MonadIO m, MonadError String m, MonadReader Container m) => String -> FilePath -> FilePath -> m ()
lxcFilePush mode src dst = do
    c <- ask
    lxc ["file", "push", "--mode=" <> pack mode, pack src, c <> "/" <> pack dst]
