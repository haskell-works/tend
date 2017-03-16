{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}

module Main where

import Control.Lens
import Data.Monoid
import Data.Version
import Development.GitRev
import HaskellWorks.Ci.Action.Help
import HaskellWorks.Ci.Action.FromRemote
import HaskellWorks.Ci.Action.NewPr
import HaskellWorks.Ci.Action.OpenCi
import HaskellWorks.Ci.Action.OrgRepos
import HaskellWorks.Ci.Action.Push
import HaskellWorks.Ci.Action.UserRepos
import HaskellWorks.Ci.Action.Version
import HaskellWorks.Ci.Options
import HaskellWorks.Ci.Options.Cmd
import Options.Applicative
import Paths_hwa_ci (version)
import qualified HaskellWorks.Ci.Options as O
import qualified Options.Applicative as O

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do
  options <- O.execParser (O.optionsParser version $(gitHash))
  case options ^. goptCmd of
    CmdOfCmdFromRemote  cmd -> actionFromRemote cmd
    CmdOfCmdHelp        cmd -> actionHelp       cmd
    CmdOfCmdOpenCi      cmd -> actionOpenCi     cmd
    CmdOfCmdOrgRepos    cmd -> actionOrgRepos   cmd
    CmdOfCmdNewPr       cmd -> actionNewPr      cmd
    CmdOfCmdPush        cmd -> actionPush       cmd
    CmdOfCmdUserRepos   cmd -> actionUserRepos  cmd
    CmdOfCmdVersion     cmd -> actionVersion    cmd version
