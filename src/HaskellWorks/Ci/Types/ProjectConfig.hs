{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

module HaskellWorks.Ci.Types.ProjectConfig
  ( ProjectConfig (..)
  ) where

import Dhall
import HaskellWorks.Ci.Types.VariableAssignment

data ProjectConfig = ProjectConfig
  { projectName       :: Text
  , projectOwner      :: Text
  , projectVariables  :: Vector VariableAssignment
  } deriving (Eq, Generic, Interpret, Show)
