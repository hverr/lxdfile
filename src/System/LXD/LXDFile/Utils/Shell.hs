{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Utils.Shell where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)

import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Turtle (ExitCode(..), proc)

exec :: (MonadIO m, MonadError String m) => Text -> [Text] -> Maybe Text -> m ()
exec cmd args stdin = proc cmd args stdin' >>= toErr
  where
    stdin' | Just t <- stdin = return t
           | otherwise = mempty
    toErr ExitSuccess = return ()
    toErr (ExitFailure status) = throwError $ "error: exec " ++ show (cmd:args) ++ ": return code " ++ show status

lxc :: (MonadIO m, MonadError String m) => [Text] -> m ()
lxc args = exec "lxc" args Nothing

class Monad m => HasContainer m where
    askContainer :: m Text

lxcExec :: (MonadIO m, MonadError String m, HasContainer m) => [Text] -> m ()
lxcExec args = do
    c <- askContainer
    lxc $ ["exec", "--mode=non-interactive", c, "--"] ++ args

lxcFilePush :: (MonadIO m, MonadError String m, HasContainer m) => String -> FilePath -> FilePath -> m ()
lxcFilePush mode src dst = do
    c <- askContainer
    lxc ["file", "push", "--mode=" <> pack mode, pack src, c <> "/" <> pack dst]
