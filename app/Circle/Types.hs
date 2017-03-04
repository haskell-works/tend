{-# LANGUAGE OverloadedStrings #-}

module Circle.Types where

import Control.Monad
import Data.Aeson

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
    [ "name"  .= name
    , "value" .= value
    ]
