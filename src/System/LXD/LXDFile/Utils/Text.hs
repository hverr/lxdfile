module System.LXD.LXDFile.Utils.Text where

import Data.Text (Text, pack)

showT :: Show a => a -> Text
showT = pack . show
