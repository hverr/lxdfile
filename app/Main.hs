{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Catch (MonadThrow, throwM)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import qualified Data.Text as T

import Network.LXD.Client (RemoteHost(..))
import Network.LXD.Client.Commands
    (ContainerName(..), WithLocalHost, WithRemoteHost,
     def, runWithLocalHost, runWithRemoteHost)
import Network.URI (URI(..), URIAuth(..), parseURI)

import Options.Applicative

import System.IO.Error (userError)
import qualified System.LXD.Client.Config as LXC

import Text.Read (readMaybe)

import Language.LXDFile (baseImage)
import Language.LXDFile.InitScript.Types (InitScriptError)
import Language.LXDFile.Version (version)
import System.LXD.LXDFile.Launch (Profile, InitScriptContext(..))
import System.LXD.LXDFile.Types (Image(..), Container(..), parseImage, serializeImage, parseContainer)
import qualified Language.LXDFile as LXDFile
import qualified Language.LXDFile.InitScript as InitScript
import qualified System.LXD.LXDFile as LXDFile

data InitScriptArg = InitScriptArg FilePath FilePath -- ^ Init script, context

data Command = BuildCommand FilePath Image FilePath (Maybe Image) -- ^ LXDFile, image tag, base directory, and base image
             | LaunchCommand Image Container Profile [InitScriptArg]       -- ^ Image, container, context, profile, list of init scripts
             | InjectCommand Container [InitScriptArg] -- ^ Container, init scripts
             | VersionCommand

buildCmd :: Mod CommandFields Command
buildCmd =
    command "build" $ info (helper <*> cmd') $ progDesc "build an LXD image using an LXDFile"
 where
    cmd' = BuildCommand <$> strOption (short 'f' <> metavar "LXDFILE" <> value "lxdfile" <> help "location of the lxdfile")
                        <*> argument imageOption (metavar "IMAGE" <> help "name of the newly built image")
                        <*> strArgument (metavar "DIR" <> value "." <> help "base directory")
                        <*> argument (Just <$> imageOption) (long "from" <> metavar "IMAGE" <> value Nothing <> help "override the base image")

launchCmd :: Mod CommandFields Command
launchCmd =
    command "launch" $ info (helper <*> cmd') $ progDesc "launch an LXD image with init scripts"
  where
    cmd' = LaunchCommand <$> argument imageOption (metavar "IMAGE" <> help "name of an LXD iamge")
                         <*> argument containerOption (metavar "CONTAINER" <> help "name of the created LXD container")
                         <*> option (Just <$> str) (short 'p' <> long "profile" <> value Nothing <> help "LXD profile for the launched container")
                         <*> many initScriptArg

injectCmd :: Mod CommandFields Command
injectCmd =
    command "inject" $ info (helper <*> cmd') $ progDesc "inject init scripts in running containers"
  where
    cmd' = InjectCommand <$> option containerOption (metavar "CONTAINER" <> help "name of the LXD container")
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
main = do
    lxcCfg <- LXC.parseDefaultFile
    execParser opts >>= run lxcCfg
  where
    opts = info (helper <*> subcommand) $ progDesc "Automatically build and manage LXD images and containers."

run :: LXC.Config -> Command -> IO ()
run lxcCfg (BuildCommand fp image dir base) = do
    lxdfile <- LXDFile.parseFile fp >>= orErr "parse error"
    let lxdfile' = case base of Nothing -> lxdfile
                                Just b -> lxdfile { baseImage = T.unpack $ serializeImage b }

    case imageRemote image of
        Nothing -> runLocal          $ LXDFile.build lxdfile' (imageName image) dir
        Just r -> runRemote lxcCfg r $ LXDFile.build lxdfile' (imageName image) dir

run lxcCfg (LaunchCommand image container profile isas) = do
    scripts <- (sequence <$> mapM parseInitScriptContext isas) >>= orErr "parse error"
    case containerRemote container of
        Nothing -> runLocal          $ LXDFile.launch image
                                                      (ContainerName . T.unpack $ containerName container)
                                                      profile
                                                      scripts
        Just r -> runRemote lxcCfg r $ LXDFile.launch image
                                                      (ContainerName . T.unpack $ containerName container)
                                                      profile
                                                      scripts

run lxcCfg (InjectCommand container isas) = do
    scripts <- (sequence <$> mapM parseInitScriptContext isas) >>= orErr "parse error"
    case containerRemote container of
        Nothing -> runLocal          $ LXDFile.inject (ContainerName . T.unpack $ containerName container) scripts
        Just r -> runRemote lxcCfg r $ LXDFile.inject (ContainerName . T.unpack $ containerName container) scripts

run _ VersionCommand = putStrLn $ showVersion version

runLocal :: WithLocalHost a -> IO a
runLocal = runWithLocalHost def

runRemote :: LXC.Config -> Text -> WithRemoteHost a -> IO a
runRemote LXC.Config{..} remote action' =
    case M.lookup remote configRemotes of
        Nothing -> throwM . userError $ "error: unknown remote: " ++ T.unpack remote
        Just r -> case parseURI (T.unpack $ LXC.remoteAddr r) of
            Nothing -> throwM . userError $ "error: could not parse remote address: " ++ T.unpack (LXC.remoteAddr r)
            Just URI{..} -> case uriAuthority of
                Nothing -> throwM . userError $ "error: could not parse remote address: " ++ T.unpack (LXC.remoteAddr r)
                Just URIAuth{..} ->
                    let h = def { remoteHostHost = uriRegName
                                , remoteHostPort = fromMaybe (remoteHostPort def) (readMaybe uriPort)
                                , remoteHostBasePath = uriPath }
                    in runWithRemoteHost h action'

parseInitScriptContext :: InitScriptArg -> IO (Either InitScriptError InitScriptContext)
parseInitScriptContext (InitScriptArg fp ctx) = do
    v <- InitScript.parseFile fp
    return $ InitScriptContext <$> v <*> pure ctx

imageOption :: ReadM Image
imageOption = eitherReader (A.parseOnly parseImage . T.pack)

containerOption :: ReadM Container
containerOption = eitherReader (A.parseOnly parseContainer . T.pack)

orErr :: (MonadThrow m, Show e) => String -> Either e a -> m a
orErr _ (Right v) = return v
orErr p (Left e) = throwM . userError $ p ++ ": " ++ show e
