{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.UserRepos
  ( CmdUserRepos(..)
  , parserCmdUserRepos
  ) where

import Control.Lens
import Options.Applicative

data CmdUserRepos = CmdUserRepos deriving (Show, Eq)

makeLenses ''CmdUserRepos

parserCmdUserRepos :: Parser CmdUserRepos
parserCmdUserRepos = pure CmdUserRepos
