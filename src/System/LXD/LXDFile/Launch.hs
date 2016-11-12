{-# LANGUAGE FlexibleContexts #-}
-- | Launch LXD images with init scripts
module System.LXD.LXDFile.Launch (
  launch
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)

import Language.LXDFile.InitScript (InitScript)

launch :: (MonadIO m, MonadError String m) => String -> String -> FilePath -> [InitScript] -> m ()
launch _image _container _ctx _scripts = undefined
