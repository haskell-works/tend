{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.OpenCi
  ( CmdOpenCi(..)
  , parserCmdOpenCi
  ) where

import Options.Applicative

data CmdOpenCi = CmdOpenCi deriving (Show, Eq)

parserCmdOpenCi :: Parser CmdOpenCi
parserCmdOpenCi = pure CmdOpenCi
