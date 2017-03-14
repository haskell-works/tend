{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.OrgRepos where

import Data.List
import Data.Maybe
import Data.Monoid
import HaskellWorks.Ci.Options.Cmd.OrgRepos
-- import HaskellWorks.Ci.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.IO as TIO
import qualified GitHub.Data.Repos as Github
import qualified GitHub.Endpoints.Repos as Github

actionOrgRepos :: CmdOrgRepos -> IO ()
actionOrgRepos cmd = do
  possibleRepos <- Github.organizationRepos "thoughtbot"
  case possibleRepos of
       (Left error)  -> putStrLn $ "Error: " <> show error
       (Right repos) -> putStrLn $ show repos

-- formatRepo repo =
--   (Github.repoName repo) <> "\t" <>
--     (fromMaybe "" $ Github.repoDescription repo) <> "\n" <>
--     (Github.repoHtmlUrl repo) <> "\n" <>
--     (fromMaybe "" $ Github.repoCloneUrl repo) <> "\t" <>
--     (formatDate $ Github.repoUpdatedAt repo) <> "\n" <>
--     formatLanguage (Github.repoLanguage repo) <>
--     "watchers: " <> (show $ Github.repoWatchers repo) <> "\t" <>
--     "forks: " <> (show $ Github.repoForks repo)
--
-- formatDate (Just date) = "????"
-- formatDate Nothing = "????"
--
-- formatLanguage (Just language) = "language: " <> language <> "\t"
-- formatLanguage Nothing = ""
