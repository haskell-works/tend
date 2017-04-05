{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module HaskellWorks.Ci.Types where

import Control.Lens
import Control.Monad
import Data.Aeson as J
import Dhall

-- auto :: (GenericInterpret (Rep a), Generic a, Interpret a) => Type a
-- auto = autoWith (defaultInterpretOptions { fieldModifier = TL.dropWhile (== '_') })

newtype CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Eq, Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig

data VariableAssignment = VariableAssignment
  { name  :: Text
  , value :: Text
  } deriving (Eq, Generic, Interpret, Show)

instance FromJSON VariableAssignment where
 parseJSON (Object v) = VariableAssignment
    <$> v .: "name"
    <*> v .: "value"
 parseJSON _ = mzero

instance ToJSON VariableAssignment where
  toJSON (VariableAssignment name' value') = object
    [ "name"  J..= name'
    , "value" J..= value'
    ]

data GithubRemote = GithubRemote
  { githubRemoteOrganisation :: String
  , githubRemoteProject      :: String
  } deriving (Eq, Show)

newtype CiConfig = CiConfig
  { projects :: Vector ProjectConfig
  } deriving (Eq, Generic, Interpret, Show)

data ProjectConfig = ProjectConfig
  { projectName       :: Text
  , projectOwner      :: Text
  , projectVariables  :: Vector VariableAssignment
  } deriving (Eq, Generic, Interpret, Show)
