{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Action.Version where

import qualified Data.Text.Lazy                      as LT
import           Data.Text.Lazy.IO                   as LTIO
import           Data.Version                        (Version, showVersion)
import           HaskellWorks.Ci.Options.Cmd.Version

actionVersion :: CmdVersion -> Version -> IO ()
actionVersion _ version = LTIO.putStrLn $ "hwa-ci " <> LT.pack (showVersion version) <> " "
