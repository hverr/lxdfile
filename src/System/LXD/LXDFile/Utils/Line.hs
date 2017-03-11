module System.LXD.LXDFile.Utils.Line where

import Control.Monad.IO.Class (MonadIO)

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Turtle (Line, textToLine, unsafeTextToLine, echo)

import System.LXD.LXDFile.Utils.String (replace)

showL :: Show a => a -> Line
showL = fromMaybe "" . textToLine . pack . replace "\n" " " . show

unsafeStringToLine :: String -> Line
unsafeStringToLine = unsafeTextToLine . pack

echoT :: MonadIO m => Text -> m ()
echoT = echo . unsafeTextToLine

echoS :: MonadIO m => String -> m ()
echoS = echo . unsafeStringToLine
