{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module HaskellWorks.Ci.Types.CircleConfig where

import Control.Lens
import Dhall

newtype CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Eq, Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig
