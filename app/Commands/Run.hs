{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Commands.Run where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Version (showVersion)

import Language.LXDFile (LXDFile(..))
import Language.LXDFile.InitScript.Types (InitScriptError)
import Language.LXDFile.Version (version)
import qualified Language.LXDFile as LXDFile
import qualified Language.LXDFile.InitScript as InitScript

import System.LXD.LXDFile.Launch (InitScriptContext(..))
import qualified System.LXD.LXDFile as LXDFile
import qualified System.LXD.LXDFile.Volume as Volume

import Commands.Types

type MonadRun m = (MonadIO m, MonadError String m)

class Runnable a where
    run :: MonadRun m => a -> m ()

instance Runnable Command where
    run (Build lxdfilePath destImage contextDir baseImage') = do
        lxdfile <- parseLXDFilePath lxdfilePath
        let lxdfile' = case baseImage' of Nothing -> lxdfile
                                          Just b -> lxdfile { baseImage = b }
        LXDFile.build lxdfile' destImage contextDir

    run (Launch image container profile initScripts) = do
        scripts <- parseInitScriptArguments initScripts
        LXDFile.launch image container profile scripts

    run (Inject container initScripts) = do
        scripts <- parseInitScriptArguments initScripts
        LXDFile.inject container scripts

    run (Volume cmd) = run cmd

    run Version = liftIO . putStrLn $ showVersion version

instance Runnable Volume where
    run (List container) = Volume.list container

parseLXDFilePath :: MonadRun m => LXDFilePath -> m LXDFile
parseLXDFilePath fp = liftIO (LXDFile.parseFile fp) >>= orErr "parse error"

parseInitScriptArgument :: InitScriptArgument -> IO (Either InitScriptError InitScriptContext)
parseInitScriptArgument (InitScriptArgument fp ctx) = do
    v <- InitScript.parseFile fp
    return $ InitScriptContext <$> v <*> pure ctx

parseInitScriptArguments :: MonadRun m => [InitScriptArgument] -> m [InitScriptContext]
parseInitScriptArguments args = liftIO (sequence <$> mapM parseInitScriptArgument args) >>= orErr "parse error"

orErr :: (Show e, MonadRun m) => String -> Either e a -> m a
orErr prefix = either (showErr prefix) return
  where showErr pref e = throwError $ pref ++ ": " ++ show e
