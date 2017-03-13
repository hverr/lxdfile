{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import System.Exit (exitFailure)

import Options.Applicative (execParser)

import Commands (mainParser, run)


newtype CmdT m a = CmdT { runCmdT :: ExceptT String m a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadError String)


main :: IO ()
main = execParser mainParser >>= cmd . run

cmd :: CmdT IO () -> IO ()
cmd action' = do
    x <- runExceptT $ runCmdT action'
    case x of Right () -> return ()
              Left e -> do
                putStrLn $ "error: " ++ e
                exitFailure
