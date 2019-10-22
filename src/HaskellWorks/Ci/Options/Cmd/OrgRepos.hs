{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.OrgRepos
  ( CmdOrgRepos(..)
  , parserCmdOrgRepos
  ) where

import Options.Applicative

data CmdOrgRepos = CmdOrgRepos deriving (Show, Eq)

parserCmdOrgRepos :: Parser CmdOrgRepos
parserCmdOrgRepos = pure CmdOrgRepos
