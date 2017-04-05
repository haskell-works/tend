{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

module HaskellWorks.Ci.Types.VariableAssignment
  ( VariableAssignment (..)
  ) where

import Control.Monad
import Data.Aeson as J
import Dhall

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
