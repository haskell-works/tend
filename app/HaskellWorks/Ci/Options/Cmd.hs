{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd where

import Control.Lens
import Options.Applicative
import HaskellWorks.Ci.Options.Cmd.Help       as C
import HaskellWorks.Ci.Options.Cmd.FromRemote as C
import HaskellWorks.Ci.Options.Cmd.Push       as C

data Cmd
  = CmdOfCmdPush        { _cmdPush        :: CmdPush        }
  | CmdOfCmdFromRemote  { _cmdFromRemote  :: CmdFromRemote  }
  | CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds =
      (CmdOfCmdPush       <$> subparser (command "push"         $ info parserCmdPush        $ progDesc "Push"))
  <|> (CmdOfCmdFromRemote <$> subparser (command "from-remote"  $ info parserCmdFromRemote  $ progDesc "From Remote"))
  <|> (CmdOfCmdHelp       <$> subparser (command "help"         $ info parserCmdHelp        $ progDesc "Help"))
