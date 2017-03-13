module Commands.Parser where

import Data.Monoid ((<>))

import Options.Applicative

import Commands.Types

-- ----------- --
-- MAIN PARSER --
-- ----------- --
mainParser :: ParserInfo Command
mainParser = info (helper <*> subcommand) $ progDesc "Automatically build and manage LXD images and containers."

subcommand :: Parser Command
subcommand = subparser (buildCmd <>
                        launchCmd <>
                        injectCmd <>
                        volumeCmd <>
                        versionCmd)

-- -------- --
-- COMMANDS --
-- -------- --
buildCmd :: Mod CommandFields Command
buildCmd =
    command "build" $ info (helper <*> cmd') $ progDesc "build an LXD image using an LXDFile"
 where
    cmd' = Build <$> strOption (short 'f' <> metavar "LXDFILE" <> value "lxdfile" <> help "location of the lxdfile")
                 <*> strArgument (metavar "NAME" <> help "name of the newly built image")
                 <*> strArgument (metavar "DIR" <> value "." <> help "base directory")
                 <*> option (Just <$> str) (long "from" <> metavar "IMAGE" <> value Nothing <> help "override the base image")

launchCmd :: Mod CommandFields Command
launchCmd =
    command "launch" $ info (helper <*> cmd') $ progDesc "launch an LXD image with init scripts"
  where
    cmd' = Launch <$> strArgument (metavar "IMAGE" <> help "name of an LXD iamge")
                  <*> strArgument (metavar "CONTAINER" <> help "name of the created LXD container")
                  <*> option (Just <$> str) (short 'p' <> long "profile" <> value Nothing <> help "LXD profile for the launched container")
                  <*> many initScriptArgument

injectCmd :: Mod CommandFields Command
injectCmd =
    command "inject" $ info (helper <*> cmd') $ progDesc "inject init scripts in running containers"
  where
    cmd' = Inject <$> strArgument (metavar "CONTAINER" <> help "name of the LXD container")
                  <*> many initScriptArgument

volumeCmd :: Mod CommandFields Command
volumeCmd =
    command "volume" $ info (helper <*> cmd') $ progDesc "work with lxdfile volumes"
  where
    cmd' = subparser volumeListCmd

versionCmd :: Mod CommandFields Command
versionCmd =
    command "version" $ info (helper <*> cmd') $ progDesc "show the program version"
  where
    cmd' = pure Version

-- ------- --
-- VOLUMES --
-- ------- --
volumeListCmd :: Mod CommandFields Command
volumeListCmd =
    command "list" $ info (helper <*> cmd') $ progDesc "list all volumes of a container"
  where
    cmd' = Volume . List <$> strArgument (metavar "CONTAINTER" <> help "name of the LXD container")

-- --------- --
-- ARGUMENTS --
-- --------- --
initScriptArgument :: Parser InitScriptArgument
initScriptArgument = parse <$> strOption (short 'i' <> metavar "SCRIPT[:CTX]" <> help "init script with optional context")
  where
    parse x = case break (== ':') x of
        (a, []) -> InitScriptArgument a "."
        (a, [_]) -> InitScriptArgument a "."
        (a, _:b) -> InitScriptArgument a b

