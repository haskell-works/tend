{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.Help
  ( CmdHelp(..)
  , parserCmdHelp
  ) where

import Options.Applicative

data CmdHelp = CmdHelp deriving (Show, Eq)

parserCmdHelp :: Parser CmdHelp
parserCmdHelp = pure CmdHelp
