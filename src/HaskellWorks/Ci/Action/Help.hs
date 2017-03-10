{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.Help where

import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Options.Cmd.Help

actionHelp :: CmdHelp -> IO ()
actionHelp cmd = LTIO.putStrLn "No help yet"
