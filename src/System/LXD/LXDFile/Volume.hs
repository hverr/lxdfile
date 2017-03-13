{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Volume where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Language.LXDFile as LXDFile

import System.LXD.LXDFile.Container (localContainerLXDFile)

list :: (MonadIO m, MonadError String m) => String -> m [FilePath]
list c = LXDFile.volumes <$> localContainerLXDFile c

list_ :: (MonadIO m, MonadError String m) => String -> m ()
list_ c = list c >>= mapM_ (liftIO . putStrLn)
