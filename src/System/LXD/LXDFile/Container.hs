{-# LANGUAGE FlexibleContexts #-}
module System.LXD.LXDFile.Container where

import Control.Exception (IOException, catch)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as Char8

import Language.LXDFile (LXDFile)

import System.FilePath ((</>))

type Container = String
type Remote = String

splitContainer :: Container -> (Remote, Container)
splitContainer x = case break (== ':') x of
    (c, []) -> ("", c)
    ("", _:c) -> ("", c)
    (r, _:c)  -> (r, c)

containerIsLocal :: Container -> Bool
containerIsLocal c =
    case splitContainer c of ("", _) -> True
                             ("local", _) -> True
                             _ -> False

localContainerLXDFile :: (MonadIO m, MonadError String m) => Container -> m LXDFile
localContainerLXDFile c = do
    bs <- Char8.pack <$> readLocalContainerFile c "/etc/lxdfile/lxdfile"
    decodeOr ("could not decode lxdfile of " ++ c) bs
  where
    decodeOr s x = case decode x of Just v -> return v
                                    Nothing -> throwError s

localContainerName :: MonadError String m => Container -> m Container
localContainerName c | containerIsLocal c = return . snd . splitContainer $ c
                     | otherwise = throwError $ "error: not a local container: '" ++ c ++ "'"

containerFilePath :: MonadError String m => Container -> FilePath -> m FilePath
containerFilePath c fp = do
    name <- localContainerName c
    return $ "/var/lib/lxd/containers" </> name </> "rootfs" </> sanitize fp
  where
    sanitize ('/':xs) = xs
    sanitize xs = xs

readLocalContainerFile :: (MonadIO m, MonadError String m) => Container -> FilePath -> m String
readLocalContainerFile c fp = do
    bsOrExc <- containerFilePath c fp >>= liftIO . readFile'
    case bsOrExc of Left e -> throwError e
                    Right bs -> return bs
  where
    readFile' x  = catch (Right <$> readFile x) $ \e -> return (Left $ readErr e)

    readErr :: IOException -> String
    readErr e = "could not read " ++ fp ++ " of " ++ c ++ ": " ++ show e
