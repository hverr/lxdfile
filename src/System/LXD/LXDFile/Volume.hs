{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Volume where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Language.LXDFile as LXDFile

import System.LXD.LXDFile.Container (localContainerLXDFile)

list :: (MonadIO m, MonadError String m) => String -> m ()
list c = do
    vols <- LXDFile.volumes <$> localContainerLXDFile c
    mapM_ printVolume vols
  where
    printVolume = liftIO . putStrLn
