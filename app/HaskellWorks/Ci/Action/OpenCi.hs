{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module HaskellWorks.Ci.Action.OpenCi where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Git
import HaskellWorks.Ci.Options.Cmd.OpenCi
import HaskellWorks.Ci.Text
import Web.Browser
import qualified Data.Text.Lazy as LT

splitAliasBranch :: Text -> Maybe (Text, Text)
splitAliasBranch fullBranch = case LT.splitOn "/" fullBranch of
  [alias, branch] -> Just (alias, branch)
  _               -> Nothing

actionOpenCi :: CmdOpenCi -> IO ()
actionOpenCi cmd = do
  trackingBranch <- gitTrackingBranch
  case splitAliasBranch trackingBranch of
    Just (alias, branch) -> do
      remotes <- gitRemotes
      case listToMaybe (filter (\x -> fst x == alias) remotes) of
        Just (_, GithubRemote {..}) -> do
          let url = "https://circleci.com/gh/" <> githubRemoteOrganisation <> "/" <> githubRemoteProject <> "/tree/" <> branch
          result <- openBrowser (LT.unpack url)
          unless result $ LTIO.putStrLn $ "Unable to open " <> url
        Nothing -> LTIO.putStrLn "No matching remotes"
    Nothing -> LTIO.putStrLn "Current branch invalid"
