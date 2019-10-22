{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.FromRemote
  ( CmdFromRemote(..)
  , parserCmdFromRemote
  ) where

import Options.Applicative

data CmdFromRemote = CmdFromRemote deriving (Show, Eq)

parserCmdFromRemote :: Parser CmdFromRemote
parserCmdFromRemote = pure CmdFromRemote
