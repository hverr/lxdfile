module System.LXD.LXDFile.Utils.Monad where

import Control.Monad.Except (MonadError, throwError)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow (Just x) _ = return x
orThrow Nothing  e = throwError e

orThrowM :: (Monad m, MonadError e m) => m (Maybe a) -> e -> m a
orThrowM action e = action >>= (`orThrow` e)
