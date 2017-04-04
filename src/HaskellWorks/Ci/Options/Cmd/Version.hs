{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.Version
  ( CmdVersion(..)
  , parserCmdVersion
  ) where

import Control.Lens
import Options.Applicative

data CmdVersion = CmdVersion deriving (Show, Eq)

makeLenses ''CmdVersion

parserCmdVersion :: Parser CmdVersion
parserCmdVersion = pure CmdVersion
