{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Commands.Version
  ( cmdVersion
  ) where

import Data.Version        (Version, showVersion)
import Options.Applicative

import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.IO              as LTIO
import qualified HaskellWorks.Ci.Commands.Types as Z

runVersion :: Version -> Z.VersionOptions -> IO ()
runVersion version _ = LTIO.putStrLn $ "hwa-ci " <> LT.pack (showVersion version) <> " "

optsVersion :: Parser Z.VersionOptions
optsVersion = pure Z.VersionOptions

cmdVersion :: Version -> Mod CommandFields (IO ())
cmdVersion version = command "version"  $ flip info idm $ runVersion version <$> optsVersion
