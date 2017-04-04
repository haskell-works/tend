{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.OpenCi
  ( CmdOpenCi(..)
  , parserCmdOpenCi
  ) where

import Control.Lens
import Options.Applicative

data CmdOpenCi = CmdOpenCi deriving (Show, Eq)

makeLenses ''CmdOpenCi

parserCmdOpenCi :: Parser CmdOpenCi
parserCmdOpenCi = pure CmdOpenCi
