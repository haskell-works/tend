{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Map
import Data.Text.Lazy
import Dhall
import Lib
import Network.Wreq
import qualified Network.Wreq as W
import System.Directory
import qualified Data.Text as ST
import Git

data CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig

getMe :: W.Options -> IO ()
getMe opts = do
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)

setEnv :: W.Options -> IO ()
setEnv opts = do
  let vars = fromList
        [ ("name", "foo")
        , ("value", "bar")
        ] :: Map String String
  r <- postWith opts "https://circleci.com/api/v1.1/project/github/pico-works/pico-disposal/envvar" (toJSON vars)
  print r

main :: IO ()
main = do
  home <- pack <$> getHomeDirectory
  circleConfig <- input auto (home `append` "/.circle/config")
  print (circleConfig :: CircleConfig)
  let apiToken' = toStrict (apiToken circleConfig)
  let opts = defaults & param "circle-token" .~ [apiToken']
  getMe opts

  return ()
