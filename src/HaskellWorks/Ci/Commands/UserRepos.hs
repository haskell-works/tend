{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Commands.UserRepos
  ( cmdUserRepos
  ) where

import Control.Monad
import Data.List.Extra
import Options.Applicative
import System.Directory

import qualified Data.ByteString.Char8          as BSC8
import qualified Data.Text.IO                   as T
import qualified GitHub.Auth                    as Auth
import qualified GitHub.Data.Repos              as Github
import qualified GitHub.Endpoints.Repos         as Github
import qualified HaskellWorks.Ci.Commands.Types as Z

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

runUserRepos :: Z.UserReposOptions -> IO ()
runUserRepos _ = do
  home <- getHomeDirectory
  authString <- trim <$> readFile (home <> "/.github/dev-token")
  let auth = Auth.OAuth (BSC8.pack authString)
  possibleRepos <- Github.currentUserRepos auth Github.RepoPublicityAll
  case possibleRepos of
    (Left e)      -> putStrLn $ "Error: " <> show e
    (Right repos) -> forM_ repos $ \repo -> do
      forM_ (Github.repoSshUrl repo) $ \sshUrl -> do
        T.putStrLn (Github.getUrl sshUrl)

optsUserRepos :: Parser Z.UserReposOptions
optsUserRepos = pure Z.UserReposOptions

cmdUserRepos :: Mod CommandFields (IO ())
cmdUserRepos = command "user-repos"  $ flip info idm $ runUserRepos <$> optsUserRepos
