{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TemplateHaskell        #-}

module HaskellWorks.Ci.Types.CircleConfig where

import Control.Lens
import Dhall hiding (auto)

newtype CircleConfig = CircleConfig
  { _apiToken :: Text
  } deriving (Eq, Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig
