{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.OrgRepos
  ( CmdOrgRepos(..)
  , parserCmdOrgRepos
  ) where

import Control.Lens
import Options.Applicative

data CmdOrgRepos = CmdOrgRepos deriving (Show, Eq)

makeLenses ''CmdOrgRepos

parserCmdOrgRepos :: Parser CmdOrgRepos
parserCmdOrgRepos = pure CmdOrgRepos
