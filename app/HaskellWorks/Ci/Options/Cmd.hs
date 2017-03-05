{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd where

import Control.Lens
import Options.Applicative
import HaskellWorks.Ci.Options.Cmd.Help as C
import HaskellWorks.Ci.Options.Cmd.Push as C

data Cmd
  = CmdOfCmdPush { _cmdPush  :: CmdPush  }
  | CmdOfCmdHelp { _cmdHelp  :: CmdHelp  }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds =
      (CmdOfCmdHelp <$> subparser (command "help" $ info parserCmdHelp $ progDesc "Help"))
  <|> (CmdOfCmdPush <$> subparser (command "push" $ info parserCmdPush $ progDesc "Push"))
