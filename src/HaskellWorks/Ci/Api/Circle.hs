{-# LANGUAGE OverloadedStrings    #-}

module HaskellWorks.Ci.Api.Circle where

import Control.Lens
import Data.Aeson
import Data.Either.Combinators
import Data.Monoid
import Data.Text.Lazy
import HaskellWorks.Ci.Types
import Network.Wreq
import qualified Network.Wreq as W

getMe :: W.Options -> IO ()
getMe opts = do
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)

postCircleEnv :: W.Options -> Text -> Text -> Text -> VariableAssignment -> IO (Either Text VariableAssignment)
postCircleEnv opts vcsType username project assignment = do
  let url = "https://circleci.com/api/v1.1/project/" <> vcsType <> "/" <> username <> "/" <> project <> "/envvar"
  r <- postWith opts (unpack url) (toJSON assignment)
  case r ^. responseStatus . statusCode of
    code | code >= 200 && code < 300  -> return (mapLeft pack (eitherDecode (r ^. responseBody) :: Either String VariableAssignment))
    _                                 -> return (Left "Error")

getCircleEnv :: W.Options -> Text -> Text -> Text -> IO (Either Text [VariableAssignment])
getCircleEnv opts vcsType username project = do
  let url = "https://circleci.com/api/v1.1/project/" <> vcsType <> "/" <> username <> "/" <> project <> "/envvar"
  r <- getWith opts (unpack url)
  case r ^. responseStatus . statusCode of
    code | code >= 200 && code < 300  -> return (mapLeft pack (eitherDecode (r ^. responseBody) :: Either String [VariableAssignment]))
    _                                 -> return (Left "Error")
