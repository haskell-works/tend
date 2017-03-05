{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.Push
  ( CmdPush(..)
  , parserCmdPush
  ) where

import Control.Lens
import Data.Text.Lazy
import HaskellWorks.Ci.Options.Internal
import Options.Applicative

data CmdPush = CmdPush deriving (Show, Eq)

makeLenses ''CmdPush

parserCmdPush :: Parser CmdPush
parserCmdPush = pure CmdPush
