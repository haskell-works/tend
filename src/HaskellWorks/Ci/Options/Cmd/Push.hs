{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.Push
  ( CmdPush(..)
  , parserCmdPush
  ) where

import Options.Applicative

data CmdPush = CmdPush deriving (Show, Eq)

parserCmdPush :: Parser CmdPush
parserCmdPush = pure CmdPush
