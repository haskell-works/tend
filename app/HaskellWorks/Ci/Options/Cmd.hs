{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd where

import Control.Lens
import Data.Monoid
import Options.Applicative
import HaskellWorks.Ci.Options.Cmd.FromRemote as C
import HaskellWorks.Ci.Options.Cmd.Help       as C
import HaskellWorks.Ci.Options.Cmd.NewPr      as C
import HaskellWorks.Ci.Options.Cmd.OpenCi     as C
import HaskellWorks.Ci.Options.Cmd.Push       as C
import HaskellWorks.Ci.Options.Cmd.Version    as C

data Cmd
  = CmdOfCmdFromRemote  { _cmdFromRemote  :: CmdFromRemote  }
  | CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  | CmdOfCmdNewPr       { _cmdNewPr       :: CmdNewPr       }
  | CmdOfCmdOpenCi      { _cmdOpenCi      :: CmdOpenCi      }
  | CmdOfCmdPush        { _cmdPush        :: CmdPush        }
  | CmdOfCmdVersion     { _cmdVersion     :: CmdVersion     }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds = subparser
  (   command "from-remote" (info (CmdOfCmdFromRemote <$> parserCmdFromRemote) $ progDesc "From Remote"         )
  <>  command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                )
  <>  command "new-pr"      (info (CmdOfCmdNewPr      <$> parserCmdNewPr     ) $ progDesc "New PR"              )
  <>  command "open-ci"     (info (CmdOfCmdOpenCi     <$> parserCmdOpenCi    ) $ progDesc "Open CI in browser"  )
  <>  command "push"        (info (CmdOfCmdPush       <$> parserCmdPush      ) $ progDesc "Push"                )
  <>  command "version"     (info (CmdOfCmdVersion    <$> parserCmdVersion   ) $ progDesc "Version"             )
  )
