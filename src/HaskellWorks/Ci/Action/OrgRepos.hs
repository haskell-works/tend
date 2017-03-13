{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.OrgRepos where

import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Options.Cmd.OrgRepos

actionOrgRepos :: CmdOrgRepos -> IO ()
actionOrgRepos cmd = LTIO.putStrLn "No help yet"
