{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.Version where

import Data.Monoid
import Data.Text.Lazy.IO as LTIO
import Data.Version (showVersion, Version)
import HaskellWorks.Ci.Options.Cmd.Version
import qualified Data.Text.Lazy as LT

actionVersion :: CmdVersion -> Version -> IO ()
actionVersion cmd version = LTIO.putStrLn $ "hwa-ci " <> LT.pack (showVersion version)
