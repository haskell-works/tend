{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

module HaskellWorks.Ci.Types.GithubRemote
  ( GithubRemote        (..)
  ) where

data GithubRemote = GithubRemote
  { githubRemoteOrganisation :: String
  , githubRemoteProject      :: String
  } deriving (Eq, Show)
