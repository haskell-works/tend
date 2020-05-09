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
import qualified GitHub                         as GH
import qualified HaskellWorks.Ci.Commands.Types as Z

{- HLINT ignore "Redundant do"    -}

runUserRepos :: Z.UserReposOptions -> IO ()
runUserRepos _ = do
  home <- getHomeDirectory
  authString <- trim <$> readFile (home <> "/.github/dev-token")
  let auth = GH.OAuth (BSC8.pack authString)
  possibleRepos <- GH.github auth $ GH.currentUserReposR GH.RepoPublicityAll 1000
  case possibleRepos of
    (Left e)      -> putStrLn $ "Error: " <> show e
    (Right repos) -> forM_ repos $ \repo -> do
      forM_ (GH.repoSshUrl repo) $ \sshUrl -> do
        T.putStrLn (GH.getUrl sshUrl)

optsUserRepos :: Parser Z.UserReposOptions
optsUserRepos = pure Z.UserReposOptions

cmdUserRepos :: Mod CommandFields (IO ())
cmdUserRepos = command "user-repos"  $ flip info idm $ runUserRepos <$> optsUserRepos
