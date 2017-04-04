{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module HaskellWorks.Ci.Options.Cmd.NewPr
  ( CmdNewPr(..)
  , parserCmdNewPr
  ) where

import Control.Lens
import Options.Applicative

data CmdNewPr = CmdNewPr deriving (Show, Eq)

makeLenses ''CmdNewPr

parserCmdNewPr :: Parser CmdNewPr
parserCmdNewPr = pure CmdNewPr
