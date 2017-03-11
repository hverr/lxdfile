{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Monoid ((<>))
import Data.Version (showVersion)

import Options.Applicative

import System.Exit (exitFailure)

import Language.LXDFile (Image, baseImage)
import Language.LXDFile.InitScript.Types (InitScriptError)
import Language.LXDFile.Version (version)
import System.LXD.LXDFile.Launch (Profile, InitScriptContext(..))
import qualified Language.LXDFile as LXDFile
import qualified Language.LXDFile.InitScript as InitScript
import qualified System.LXD.LXDFile as LXDFile

data InitScriptArg = InitScriptArg FilePath FilePath -- ^ Init script, context

data Command = BuildCommand FilePath String FilePath (Maybe Image) -- ^ LXDFile, image tag, base directory, and base image
             | LaunchCommand String String Profile [InitScriptArg]       -- ^ Image, container, context, profile, list of init scripts
             | InjectCommand String [InitScriptArg] -- ^ Container, init scripts
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
                        <*> option (Just <$> str) (long "from" <> metavar "IMAGE" <> value Nothing <> help "override the base image")

launchCmd :: Mod CommandFields Command
launchCmd =
    command "launch" $ info (helper <*> cmd') $ progDesc "launch an LXD image with init scripts"
  where
    cmd' = LaunchCommand <$> strArgument (metavar "IMAGE" <> help "name of an LXD iamge")
                         <*> strArgument (metavar "CONTAINER" <> help "name of the created LXD container")
                         <*> option (Just <$> str) (short 'p' <> long "profile" <> value Nothing <> help "LXD profile for the launched container")
                         <*> many initScriptArg

injectCmd :: Mod CommandFields Command
injectCmd =
    command "inject" $ info (helper <*> cmd') $ progDesc "inject init scripts in running containers"
  where
    cmd' = InjectCommand <$> strArgument (metavar "CONTAINER" <> help "name of the LXD container")
                         <*> many initScriptArg

initScriptArg :: Parser InitScriptArg
initScriptArg = parse <$> strOption (short 'i' <> metavar "SCRIPT[:CTX]" <> help "init script with optional context")
  where
    parse x = case break (== ':') x of
        (a, []) -> InitScriptArg a "."
        (a, [_]) -> InitScriptArg a "."
        (a, _:b) -> InitScriptArg a b

versionCmd :: Mod CommandFields Command
versionCmd =
    command "version" $ info (helper <*> cmd') $ progDesc "show the program version"
  where
    cmd' = pure VersionCommand

subcommand :: Parser Command
subcommand = subparser (buildCmd <> launchCmd <> injectCmd <> versionCmd)

main :: IO ()
main =
    execParser opts >>= cmd . run
  where
    opts = info (helper <*> subcommand) $ progDesc "Automatically build and manage LXD images and containers."

cmd :: CmdT IO () -> IO ()
cmd action' = do
    x <- runExceptT $ runCmdT action'
    case x of Right () -> return ()
              Left e -> do
                putStrLn $ "error: " ++ e
                exitFailure

run :: (MonadIO m, MonadError String m) => Command -> m ()
run (BuildCommand fp name dir base) = do
    lxdfile <- liftIO (LXDFile.parseFile fp) >>= orErr "parse error"
    let lxdfile' = case base of Nothing -> lxdfile
                                Just b -> lxdfile { baseImage = b }
    LXDFile.build lxdfile' name dir
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e

run (LaunchCommand image container profile isas) = do
    scripts <- liftIO (sequence <$> mapM parseInitScriptContext isas) >>= orErr "parse error"
    LXDFile.launch image container profile scripts
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e

run (InjectCommand name isas) = do
    scripts <- liftIO (sequence <$> mapM parseInitScriptContext isas) >>= orErr "parse error"
    LXDFile.inject name scripts
  where
    orErr pref = either (showErr pref) return
    showErr pref e = throwError $ pref ++ ": " ++ show e

run VersionCommand = liftIO . putStrLn $ showVersion version

parseInitScriptContext :: InitScriptArg -> IO (Either InitScriptError InitScriptContext)
parseInitScriptContext (InitScriptArg fp ctx) = do
    v <- InitScript.parseFile fp
    return $ InitScriptContext <$> v <*> pure ctx
