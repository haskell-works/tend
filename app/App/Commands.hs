module App.Commands where

import Data.Semigroup                      ((<>))
import HaskellWorks.Ci.Commands.FromRemote
import HaskellWorks.Ci.Commands.Help
import HaskellWorks.Ci.Commands.NewPr
import HaskellWorks.Ci.Commands.OpenCi
import HaskellWorks.Ci.Commands.OrgRepos
import HaskellWorks.Ci.Commands.UserRepos
import HaskellWorks.Ci.Commands.Version
import Options.Applicative
import Paths_hwa_ci                        (version)

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdFromRemote
  <>  cmdHelp
  <>  cmdNewPr
  <>  cmdOpenCi
  <>  cmdOrgRepos
  <>  cmdUserRepos
  <>  cmdVersion version
