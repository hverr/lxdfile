module System.LXD.LXDFile.ScriptAction where

import Control.Lens (Lens', lens, (^.), (.~))
import Control.Monad.State (State, evalState, modify, get)

import Data.Foldable (foldlM)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Language.LXDFile.Types (Action(..), Arguments(..), Source, Destination)
import System.LXD.LXDFile.Utils.String (replace)

data ScriptCtx = ScriptCtx { _currentDirectory :: FilePath
                           , _environment :: [(String, String)] }

currentDirectory :: Lens' ScriptCtx FilePath
currentDirectory = lens _currentDirectory $ \s x -> s { _currentDirectory = x }

environment :: Lens' ScriptCtx [(String, String)]
environment = lens _environment $ \s x -> s { _environment = x }

defaultScriptCtx :: ScriptCtx
defaultScriptCtx = ScriptCtx "/" []

data ScriptAction = SRun ScriptCtx Arguments
                  | SCopy ScriptCtx Source Destination
                  | SNOOP

scriptActions :: [Action] -> [ScriptAction]
scriptActions actions = reverse $ evalState (foldlM f [] actions) defaultScriptCtx
  where f xs a = (: xs) <$> scriptAction a

scriptAction :: Action -> State ScriptCtx ScriptAction
scriptAction (ChangeDirectory fp) = do
    modify $ \s -> (currentDirectory .~ fp) s
    return SNOOP
scriptAction (Copy src dst) = SCopy <$> get <*> pure src <*> pure dst
scriptAction (Run x) = SRun <$> get <*> pure x

currentDirectorySh :: ScriptCtx -> [Text]
currentDirectorySh ctx = ["cd " <> pack (ctx ^. currentDirectory)]

environmentSh :: ScriptCtx -> [Text]
environmentSh ctx = map conv $ ctx ^. environment
   where conv (key, value) = pack key <> "=" <> pack value

copyDest :: ScriptCtx -> Destination -> Destination
copyDest _ d@('/':_) = d
copyDest ctx dir = (ctx ^. currentDirectory) <> "/" <> dir

argumentsSh :: Arguments -> [Text]
argumentsSh (ArgumentsShell s) = [pack s]
argumentsSh (ArgumentsList xs) = [pack . unwords $ map escapeArg xs]
  where
    escapeArg = replace " " "\\ " . replace "\t" "\\\t" . replace "\"" "\\\"" . replace "'" "\\'"
