{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.FromRemote where

import Data.Maybe
import Data.Monoid
import Data.Text.Lazy as LT
import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Api.Circle
import HaskellWorks.Ci.Options
import HaskellWorks.Ci.Options.Cmd.FromRemote
import HaskellWorks.Ci.Types
import Prelude hiding (lines)
import System.Process
import qualified Data.List.Extra          as LE
import qualified HaskellWorks.Ci.Options  as O
import qualified Options.Applicative      as O
import qualified Prelude                  as P

-- | Conversion from a `Show` constrained type to `Text`
tshow :: Show a => a -> Text
tshow = pack . show

remoteEntriesFromLine :: String -> Maybe (String, GithubRemote)
remoteEntriesFromLine s = case LE.split (== ' ') (LE.replace "\t" " " s) of
  alias : uri : _ : xs -> case LE.split (== ':') uri of
    [userHost, uriPath] -> if userHost == "git@github.com"
      then case LE.split (== '/') uriPath of
        [organisation, gitFile] -> case LE.split (== '.') gitFile of
          [project, "git"] -> Just (alias, GithubRemote organisation project)
          _ -> Nothing
        _ -> Nothing
      else Nothing
  _ -> Nothing

actionFromRemote :: CmdFromRemote -> IO ()
actionFromRemote cmd = do
  remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
  let remoteEntries = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
  LTIO.putStrLn $ "Remote entries: " <> tshow remoteEntries
