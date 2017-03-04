{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Circle.Types where

import Control.Lens
import Control.Monad
import Data.Aeson as J
import Dhall

newtype CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig

data VariableAssignment = VariableAssignment
  { name :: String
  , value :: String
  } deriving (Eq, Show)

instance FromJSON VariableAssignment where
 parseJSON (Object v) = VariableAssignment
    <$> v .: "name"
    <*> v .: "value"
 parseJSON _ = mzero

instance ToJSON VariableAssignment where
  toJSON (VariableAssignment name value) = object
    [ "name"  J..= name
    , "value" J..= value
    ]

data GithubRemote = GithubRemote
  { githubRemoteOrganisation :: String
  , githubRemoteProject      :: String
  } deriving (Eq, Show)
