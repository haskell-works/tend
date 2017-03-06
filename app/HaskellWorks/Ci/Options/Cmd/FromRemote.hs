{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.FromRemote
  ( CmdFromRemote(..)
  , parserCmdFromRemote
  ) where

import Control.Lens
import Data.Text.Lazy
import HaskellWorks.Ci.Options.Internal
import Options.Applicative

data CmdFromRemote = CmdFromRemote deriving (Show, Eq)

makeLenses ''CmdFromRemote

parserCmdFromRemote :: Parser CmdFromRemote
parserCmdFromRemote = pure CmdFromRemote
