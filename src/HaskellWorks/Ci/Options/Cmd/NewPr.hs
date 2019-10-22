{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Options.Cmd.NewPr
  ( CmdNewPr(..)
  , parserCmdNewPr
  ) where

import Options.Applicative

data CmdNewPr = CmdNewPr deriving (Show, Eq)

parserCmdNewPr :: Parser CmdNewPr
parserCmdNewPr = pure CmdNewPr
