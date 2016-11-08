{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Options.Applicative

import System.Exit (exitFailure)

import Language.LXDFile (parseFile)

data Command = BuildCommand FilePath String FilePath -- ^ LXDFile, image tag and base directory

newtype CmdT m a = CmdT { runCmdT :: ExceptT String m a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

buildCmd :: Mod CommandFields Command
buildCmd =
    command "build" $ info (helper <*> cmd) $ progDesc "build an LXD image using an LXDFile"
 where
    cmd = BuildCommand <$> strOption (short 'f' <> metavar "LXDFILE" <> value "lxdfile" <> help "location of the lxdfile")
                       <*> strArgument (metavar "NAME" <> help "name of the newly built image")
                       <*> strArgument (metavar "DIR" <> value "." <> help "base directory")

subcommand :: Parser Command
subcommand = subparser buildCmd

main :: IO ()
main =
    execParser opts >>= run
  where
    opts = info (helper <*> subcommand) $ progDesc "Automatically build and manage LXD images and containers."
    run (BuildCommand lxdfile tag base) = cmd $ build lxdfile tag base

cmd :: CmdT IO () -> IO ()
cmd action = do
    x <- runExceptT $ runCmdT action
    case x of Right () -> return ()
              Left e -> do
                putStrLn $ "error: " ++ e
                exitFailure

build :: (MonadIO m, MonadError String m) => FilePath -> String -> FilePath -> m ()
build fp name dir = do
    lxdfile <- liftIO (parseFile fp) >>= orErr "parse error"
    liftIO $ putStrLn $ "Building " ++ name ++ " with " ++ fp ++ " using context " ++ dir
    liftIO $ print lxdfile
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e
