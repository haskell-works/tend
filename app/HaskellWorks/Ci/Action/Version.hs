{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.Version where

import Data.Monoid
import Data.Text.Lazy.IO as LTIO
import Data.Version (showVersion)
import HaskellWorks.Ci.Options.Cmd.Version
import Paths_hwa_ci (version)
import qualified Data.Text.Lazy as LT

actionVersion :: CmdVersion -> IO ()
actionVersion cmd = LTIO.putStrLn $ "hwa-ci " <> LT.pack (showVersion version)
