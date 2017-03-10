{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.FromRemote where

import Data.Maybe
import Data.Monoid
import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Git
import HaskellWorks.Ci.Options
import HaskellWorks.Ci.Options.Cmd.FromRemote
import HaskellWorks.Ci.Text
import Prelude hiding (lines)
import System.Process
import qualified Data.List.Extra          as LE
import qualified Prelude                  as P

actionFromRemote :: CmdFromRemote -> IO ()
actionFromRemote cmd = do
  remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
  let remoteEntries = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
  LTIO.putStrLn $ "Remote entries: " <> tshow remoteEntries
