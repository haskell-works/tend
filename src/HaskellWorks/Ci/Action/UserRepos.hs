{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Action.UserRepos where

import           Control.Monad
import qualified Data.ByteString.Char8                 as BSC8
import           Data.List.Extra
import qualified Data.Text.IO                          as T
import qualified GitHub.Auth                           as Auth
import qualified GitHub.Data.Repos                     as Github
import qualified GitHub.Endpoints.Repos                as Github
import           HaskellWorks.Ci.Options.Cmd.UserRepos
import           System.Directory

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

actionUserRepos :: CmdUserRepos -> IO ()
actionUserRepos _ = do
  home <- getHomeDirectory
  authString <- trim <$> readFile (home <> "/.github/dev-token")
  let auth = Auth.OAuth (BSC8.pack authString)
  possibleRepos <- Github.currentUserRepos auth Github.RepoPublicityAll
  case possibleRepos of
    (Left e)      -> putStrLn $ "Error: " <> show e
    (Right repos) -> forM_ repos $ \repo -> do
      forM_ (Github.repoSshUrl repo) $ \sshUrl -> do
        T.putStrLn (Github.getUrl sshUrl)
