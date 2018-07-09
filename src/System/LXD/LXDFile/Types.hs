{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module System.LXD.LXDFile.Types where


import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as T

import Network.LXD.Client.Commands (ContainerSource(..), LocalImageByAlias(..), ImageAliasName(..), RemoteImage(..), remoteImage)

import qualified System.LXD.Client.Config as LXC

-- | An image specified on the command line.
data Image = Image {
    imageRemote :: Maybe Text
  , imageName :: Text
  } deriving (Show)

-- | Parse an image from the command line.
--
-- E.g. @images:ubuntu/16.04@, @ubuntu/16.04@
parseImage :: A.Parser Image
parseImage = do
    v <- A.takeWhile (/= ':')
    A.peekChar >>= \case
        Nothing -> return $ Image Nothing v
        Just _ -> do
            _ <- A.char ':'
            n <- A.takeWhile (const True)
            return $ Image (Just v) n

serializeImage :: Image -> Text
serializeImage Image{..} = case imageRemote of Just r -> r <> ":" <> imageName
                                               Nothing -> imageName

containerSourceFromImage :: LXC.Config -> Image -> Either String ContainerSource
containerSourceFromImage LXC.Config{..} Image{..} = case imageRemote of
    Nothing -> Right
             . ContainerSourceLocalByAlias
             . LocalImageByAlias
             . ImageAliasName
             $ T.unpack imageName
    Just r -> case Map.lookup r configRemotes of
        Nothing -> Left $ "unknown remote: " ++ T.unpack r
        Just addr ->
            let i' = remoteImage (T.unpack $ LXC.remoteAddr addr)
                                 (ImageAliasName $ T.unpack imageName)
                i = i' { remoteImageProtocol = T.unpack <$> LXC.remoteProtocol addr }
            in Right $ ContainerSourceRemote i

-- | A conainer specified on the command line.
data Container = Container {
    containerRemote :: Maybe Text
  , containerName :: Text
  } deriving (Show)

-- | Parse a container from the command line.
--
-- E.g. @test@, @lxd-2:test@
parseContainer :: A.Parser Container
parseContainer = do
    v <- A.takeWhile (/= ':')
    A.peekChar >>= \case
        Nothing -> return $ Container Nothing v
        Just _ -> do
            _ <- A.char ':'
            n <- A.takeWhile (const True)
            return $ Container (Just v) n
