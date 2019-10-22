{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Commands.FromRemote
  ( cmdFromRemote
  ) where

import Data.Maybe
import Data.Monoid
import Data.Text.Lazy.IO    as LTIO
import HaskellWorks.Ci.Git
import HaskellWorks.Ci.Text
import Options.Applicative
import Prelude              hiding (lines)
import System.Process

import qualified Data.List.Extra                as LE
import qualified HaskellWorks.Ci.Commands.Types as Z
import qualified Prelude                        as P

runFromRemote :: Z.FromRemoteOptions -> IO ()
runFromRemote _ = do
  remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
  let remoteEntries = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
  LTIO.putStrLn $ "Remote entries: " <> tshow remoteEntries

optsFromRemote :: Parser Z.FromRemoteOptions
optsFromRemote = pure Z.FromRemoteOptions

cmdFromRemote :: Mod CommandFields (IO ())
cmdFromRemote = command "from-remote"  $ flip info idm $ runFromRemote <$> optsFromRemote
