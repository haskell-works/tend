{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Api.Circle where

import Control.Lens
import Network.Wreq

import qualified Network.Wreq as W

getMe :: W.Options -> IO ()
getMe opts = do
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)
