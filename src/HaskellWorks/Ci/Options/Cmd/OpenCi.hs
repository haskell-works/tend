{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.OpenCi
  ( CmdOpenCi(..)
  , parserCmdOpenCi
  ) where

import Control.Lens
import Data.Text.Lazy
import HaskellWorks.Ci.Options.Internal
import Options.Applicative

data CmdOpenCi = CmdOpenCi deriving (Show, Eq)

makeLenses ''CmdOpenCi

parserCmdOpenCi :: Parser CmdOpenCi
parserCmdOpenCi = pure CmdOpenCi
