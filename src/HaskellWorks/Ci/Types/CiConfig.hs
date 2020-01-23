{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

module HaskellWorks.Ci.Types.CiConfig
  ( CiConfig            (..)
  ) where

import Dhall
import HaskellWorks.Ci.Types.ProjectConfig

newtype CiConfig = CiConfig
  { projects :: Vector ProjectConfig
  } deriving (Eq, Generic, Interpret, Show)
