{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Ci.Commands.Types
  ( FromRemoteOptions(..)
  , HelpOptions(..)
  , NewPrOptions(..)
  , OpenCiOptions(..)
  , OrgReposOptions(..)
  , PushOptions(..)
  , UserReposOptions(..)
  , VersionOptions(..)
  ) where

import GHC.Generics

data FromRemoteOptions = FromRemoteOptions
  deriving (Eq, Generic)

data HelpOptions = HelpOptions
  deriving (Eq, Generic)

data NewPrOptions = NewPrOptions
  deriving (Eq, Generic)

data OpenCiOptions = OpenCiOptions
  deriving (Eq, Generic)

data OrgReposOptions = OrgReposOptions
  deriving (Eq, Generic)

data PushOptions = PushOptions
  deriving (Eq, Generic)

data UserReposOptions = UserReposOptions
  deriving (Eq, Generic)

data VersionOptions = VersionOptions
  deriving (Eq, Generic)
