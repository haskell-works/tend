{-# LANGUAGE DeriveAnyClass #-}

module HaskellWorks.Ci.Options.Cmd.Version
  ( CmdVersion(..)
  , parserCmdVersion
  ) where

import Options.Applicative

data CmdVersion = CmdVersion deriving (Show, Eq)

parserCmdVersion :: Parser CmdVersion
parserCmdVersion = pure CmdVersion
