module Main where

import Options.Applicative


data Command = BuildCommand FilePath String FilePath -- ^ LXDFile, image tag and base directory

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
    run (BuildCommand lxdfile tag base) = putStrLn $ "Build " ++ lxdfile ++ " " ++ tag ++ " " ++ base

