{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Git where

import Data.Maybe
import Data.Text.Lazy (Text)
import System.Process
import qualified Data.List.Extra  as LE
import qualified Data.Text.Lazy   as LT

data GithubRemote = GithubRemote
  { githubRemoteOrganisation :: Text
  , githubRemoteProject      :: Text
  } deriving (Eq, Show)

gitCurrentBranch :: IO Text
gitCurrentBranch = LT.strip . LT.pack <$> readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""

gitTrackingBranch :: IO Text
gitTrackingBranch = LT.strip . LT.pack <$> readProcess "git" ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"] ""

gitRemotes :: IO [(Text, GithubRemote)]
gitRemotes = do
  remoteLines <- lines <$> readProcess "git" ["remote", "-v"] ""
  let x = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
  return x

----

remoteEntriesFromLine :: String -> Maybe (Text, GithubRemote)
remoteEntriesFromLine s = case LE.split (== ' ') (LE.replace "\t" " " s) of
  alias : uri : _ : xs -> case LE.split (== ':') uri of
    [userHost, uriPath] -> if userHost == "git@github.com"
      then case LE.split (== '/') uriPath of
        [organisation, gitFile] -> case LE.split (== '.') gitFile of
          [project, "git"] -> Just (LT.pack alias, GithubRemote (LT.pack organisation) (LT.pack project))
          _ -> Nothing
        _ -> Nothing
      else Nothing
  _ -> Nothing
