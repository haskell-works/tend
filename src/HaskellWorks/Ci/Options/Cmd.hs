{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd where

import GHC.Generics
import HaskellWorks.Ci.Options.Cmd.FromRemote as C
import HaskellWorks.Ci.Options.Cmd.Help       as C
import HaskellWorks.Ci.Options.Cmd.NewPr      as C
import HaskellWorks.Ci.Options.Cmd.OpenCi     as C
import HaskellWorks.Ci.Options.Cmd.OrgRepos   as C
import HaskellWorks.Ci.Options.Cmd.Push       as C
import HaskellWorks.Ci.Options.Cmd.UserRepos  as C
import HaskellWorks.Ci.Options.Cmd.Version    as C
import Options.Applicative

data Cmd
  = CmdOfCmdFromRemote  { _cmdFromRemote  :: CmdFromRemote  }
  | CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  | CmdOfCmdNewPr       { _cmdNewPr       :: CmdNewPr       }
  | CmdOfCmdOpenCi      { _cmdOpenCi      :: CmdOpenCi      }
  | CmdOfCmdOrgRepos    { _cmdOrgRepos    :: CmdOrgRepos    }
  | CmdOfCmdPush        { _cmdPush        :: CmdPush        }
  | CmdOfCmdVersion     { _cmdVersion     :: CmdVersion     }
  | CmdOfCmdUserRepos   { _cmdUserRepos   :: CmdUserRepos   }
  deriving (Show, Eq, Generic)

cmds :: Parser Cmd
cmds = subparser
  (   command "from-remote" (info (CmdOfCmdFromRemote <$> parserCmdFromRemote) $ progDesc "From Remote"                 )
  <>  command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                        )
  <>  command "new-pr"      (info (CmdOfCmdNewPr      <$> parserCmdNewPr     ) $ progDesc "New PR"                      )
  <>  command "open-ci"     (info (CmdOfCmdOpenCi     <$> parserCmdOpenCi    ) $ progDesc "Open CI in browser"          )
  <>  command "org-repos"   (info (CmdOfCmdOrgRepos   <$> parserCmdOrgRepos  ) $ progDesc "Organisation repositorires"  )
  <>  command "push"        (info (CmdOfCmdPush       <$> parserCmdPush      ) $ progDesc "Push"                        )
  <>  command "user-repos"  (info (CmdOfCmdUserRepos  <$> parserCmdUserRepos ) $ progDesc "User repositorires"          )
  <>  command "version"     (info (CmdOfCmdVersion    <$> parserCmdVersion   ) $ progDesc "Version"                     )
  )
