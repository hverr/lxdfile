{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Utils.Shell where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)

import Turtle (ExitCode(..), proc)

exec :: (MonadIO m, MonadError String m) => Text -> [Text] -> Maybe Text -> m ()
exec cmd args stdin = proc cmd args stdin' >>= toErr
  where
    stdin' | Just t <- stdin = return t
           | otherwise = mempty
    toErr ExitSuccess = return ()
    toErr (ExitFailure status) = throwError $ "error: exec " ++ show (cmd:args) ++ ": return code " ++ show status
