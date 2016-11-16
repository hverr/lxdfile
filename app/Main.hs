{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Version (showVersion)

import Options.Applicative

import System.Exit (exitFailure)

import Language.LXDFile.Version (version)
import System.LXD.LXDFile.Launch (Profile)
import qualified Language.LXDFile as LXDFile
import qualified Language.LXDFile.InitScript as InitScript
import qualified System.LXD.LXDFile as LXDFile

data Command = BuildCommand FilePath String FilePath -- ^ LXDFile, image tag and base directory
             | LaunchCommand String String FilePath Profile [FilePath]       -- ^ Image, container, context, profile, list of init scripts
             | VersionCommand

newtype CmdT m a = CmdT { runCmdT :: ExceptT String m a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

buildCmd :: Mod CommandFields Command
buildCmd =
    command "build" $ info (helper <*> cmd') $ progDesc "build an LXD image using an LXDFile"
 where
    cmd' = BuildCommand <$> strOption (short 'f' <> metavar "LXDFILE" <> value "lxdfile" <> help "location of the lxdfile")
                        <*> strArgument (metavar "NAME" <> help "name of the newly built image")
                        <*> strArgument (metavar "DIR" <> value "." <> help "base directory")

launchCmd :: Mod CommandFields Command
launchCmd =
    command "launch" $ info (helper <*> cmd') $ progDesc "launch an LXD image with init scripts"
  where
    cmd' = LaunchCommand <$> strArgument (metavar "IMAGE" <> help "name of an LXD iamge")
                         <*> strArgument (metavar "CONTAINER" <> help "name of the created LXD container")
                         <*> strArgument (metavar "DIR" <> value "." <> help "base directory for init scripts")
                         <*> option (Just <$> str) (short 'p' <> long "profile" <> value Nothing <> help "LXD profile for the launched container")
                         <*> many (strOption $ short 'i' <> metavar "SCRIPT" <> help "init script to execute after launch")

versionCmd :: Mod CommandFields Command
versionCmd =
    command "version" $ info (helper <*> cmd') $ progDesc "show the program version"
  where
    cmd' = pure VersionCommand

subcommand :: Parser Command
subcommand = subparser (buildCmd <> launchCmd <> versionCmd)

main :: IO ()
main =
    execParser opts >>= run
  where
    opts = info (helper <*> subcommand) $ progDesc "Automatically build and manage LXD images and containers."
    run (BuildCommand lxdfile tag base) = cmd $ build lxdfile tag base
    run (LaunchCommand image container ctx profile inits) = cmd $ launch image container ctx profile inits
    run VersionCommand = putStrLn $ showVersion version

cmd :: CmdT IO () -> IO ()
cmd action' = do
    x <- runExceptT $ runCmdT action'
    case x of Right () -> return ()
              Left e -> do
                putStrLn $ "error: " ++ e
                exitFailure

build :: (MonadIO m, MonadError String m) => FilePath -> String -> FilePath -> m ()
build fp name dir = do
    lxdfile <- liftIO (LXDFile.parseFile fp) >>= orErr "parse error"
    LXDFile.build lxdfile name dir
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e

launch :: (MonadIO m, MonadError String m) => String -> String -> FilePath -> Profile -> [FilePath] ->  m ()
launch image container ctx profile fps = do
    scripts <- liftIO (sequence <$> mapM InitScript.parseFile fps) >>= orErr "parse error"
    LXDFile.launch image container ctx profile scripts
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e
