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
import System.Directory
import qualified Data.Text as ST

data CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig

main :: IO ()
main = do
  home <- pack <$> getHomeDirectory
  x <- input auto (home `append` "/.circle/config")
  print (x :: CircleConfig)
  let opts = defaults & param "circle-token" .~ [toStrict (apiToken x)]
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)
  let vars = fromList
        [ ("name", "foo")
        , ("value", "bar")
        ] :: Map String String
  r <- postWith opts "https://circleci.com/api/v1.1/project/github/pico-works/pico-disposal/envvar" (toJSON vars)
  print r
  return ()
