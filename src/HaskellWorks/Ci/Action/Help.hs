{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Action.Help where

import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Options.Cmd.Help

actionHelp :: CmdHelp -> IO ()
actionHelp _ = LTIO.putStrLn "No help yet"
