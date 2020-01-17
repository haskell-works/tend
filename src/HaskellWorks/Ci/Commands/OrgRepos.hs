{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Commands.OrgRepos
  ( cmdOrgRepos
  ) where

import Control.Monad
import Data.List.Extra
import Options.Applicative
import System.Directory

import qualified Data.ByteString.Char8          as BSC8
import qualified Data.Text.IO                   as T
import qualified GitHub                         as GH
import qualified HaskellWorks.Ci.Commands.Types as Z

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

runOrgRepos :: Z.OrgReposOptions -> IO ()
runOrgRepos _ = do
  home <- getHomeDirectory
  authString <- trim <$> readFile (home <> "/.github/dev-token")
  let auth = GH.OAuth (BSC8.pack authString)
  possibleRepos <- GH.github auth $ GH.currentUserReposR GH.RepoPublicityAll 1000
  case possibleRepos of
    (Left e)      -> putStrLn $ "Error: " <> show e
    (Right repos) -> forM_ repos $ \repo -> do
      forM_ (GH.repoSshUrl repo) $ \sshUrl -> do
        T.putStrLn (GH.getUrl sshUrl)

optsOrgRepos :: Parser Z.OrgReposOptions
optsOrgRepos = pure Z.OrgReposOptions

cmdOrgRepos :: Mod CommandFields (IO ())
cmdOrgRepos = command "org-repos"  $ flip info idm $ runOrgRepos <$> optsOrgRepos
