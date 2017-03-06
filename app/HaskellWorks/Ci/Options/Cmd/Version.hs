{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.Version
  ( CmdVersion(..)
  , parserCmdVersion
  ) where

import Control.Lens
import Data.Text.Lazy
import HaskellWorks.Ci.Options.Internal
import Options.Applicative

data CmdVersion = CmdVersion deriving (Show, Eq)

makeLenses ''CmdVersion

parserCmdVersion :: Parser CmdVersion
parserCmdVersion = pure CmdVersion
