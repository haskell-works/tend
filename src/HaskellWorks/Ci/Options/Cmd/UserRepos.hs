{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.UserRepos
  ( CmdUserRepos(..)
  , parserCmdUserRepos
  ) where

import Options.Applicative

data CmdUserRepos = CmdUserRepos deriving (Show, Eq)

parserCmdUserRepos :: Parser CmdUserRepos
parserCmdUserRepos = pure CmdUserRepos
