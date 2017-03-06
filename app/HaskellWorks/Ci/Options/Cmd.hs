{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd where

import Control.Lens
import Options.Applicative
import HaskellWorks.Ci.Options.Cmd.FromRemote as C
import HaskellWorks.Ci.Options.Cmd.Help       as C
import HaskellWorks.Ci.Options.Cmd.Push       as C
import HaskellWorks.Ci.Options.Cmd.Version    as C

data Cmd
  = CmdOfCmdFromRemote  { _cmdFromRemote  :: CmdFromRemote  }
  | CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  | CmdOfCmdPush        { _cmdPush        :: CmdPush        }
  | CmdOfCmdVersion     { _cmdVersion     :: CmdVersion     }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds =
      (CmdOfCmdFromRemote <$> subparser (command "from-remote"  $ info parserCmdFromRemote  $ progDesc "From Remote"))
  <|> (CmdOfCmdHelp       <$> subparser (command "help"         $ info parserCmdHelp        $ progDesc "Help"))
  <|> (CmdOfCmdPush       <$> subparser (command "push"         $ info parserCmdPush        $ progDesc "Push"))
  <|> (CmdOfCmdVersion    <$> subparser (command "version"      $ info parserCmdVersion     $ progDesc "Version"))
