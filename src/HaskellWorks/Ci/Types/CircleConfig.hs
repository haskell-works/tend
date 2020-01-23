{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Ci.Types.CircleConfig where

import Dhall hiding (auto)

newtype CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Eq, Generic, Show)

instance FromDhall CircleConfig
