{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Circle.Types
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map
import Data.Text.Lazy as LT
import Dhall
import Lib
import Network.Wreq
import qualified Network.Wreq as W
import System.Directory
import qualified Data.Text as DT
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import System.Process
import System.IO
import Prelude hiding (lines)
import qualified Prelude as P
import qualified Data.List.Extra as LE
import Parser

splitByChar :: Char -> String -> [String]
splitByChar c s = case rest of
  []     -> [chunk]
  _:rest -> chunk : splitByChar c rest
  where (chunk, rest) = P.break (==c) s

getMe :: W.Options -> IO ()
getMe opts = do
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)

getCircleEnv :: W.Options -> String -> String -> String -> IO (Either String [VariableAssignment])
getCircleEnv opts vcsType username project = do
  let url = "https://circleci.com/api/v1.1/project/" ++ vcsType ++ "/" ++ username ++ "/" ++ project ++ "/envvar"
  r <- getWith opts url
  case r ^. responseStatus . statusCode of
    200 -> return (eitherDecode (r ^. responseBody) :: Either String [VariableAssignment])
    _   -> return (Right [])

setEnv :: W.Options -> IO ()
setEnv opts = do
  let vars = fromList
        [ ("name", "foo")
        , ("value", "bar")
        ] :: Map String String
  r <- postWith opts "https://circleci.com/api/v1.1/project/github/pico-works/pico-disposal/envvar" (toJSON vars)
  print r

remoteEntriesFromLine :: String -> Maybe (String, GithubRemote)
remoteEntriesFromLine s = case LE.split (== ' ') (LE.replace "\t" " " s) of
  alias : uri : _ : xs -> case LE.split (== ':') uri of
    [userHost, uriPath] -> if userHost == "git@github.com"
      then case LE.split (== '/') uriPath of
        [organisation, gitFile] -> case LE.split (== '.') gitFile of
          [project, "git"] -> Just (alias, GithubRemote organisation project)
          _ -> Nothing
        _ -> Nothing
      else Nothing
  _ -> Nothing

main :: IO ()
main = do
  remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
  let remoteEntries = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
  putStrLn $ "Remote entries: " ++ show remoteEntries
  home <- pack <$> getHomeDirectory
  circleConfig <- input auto (home `append` "/.circle/config")
  putStrLn $ "Circle config: " ++ show (circleConfig :: CircleConfig)
  let apiToken' = toStrict (apiToken circleConfig)
  let opts = defaults & param "circle-token" .~ [apiToken'] & header "Accept" .~ ["application/json"]
  -- getMe opts

  forM_ remoteEntries $ \(username, remoteEntry) -> do
    variableAssignmentsResult <- getCircleEnv opts "github" (githubRemoteOrganisation remoteEntry) (githubRemoteProject remoteEntry)
    case variableAssignmentsResult of
      Right variableAssignments -> do
        forM_ variableAssignments $ \variableAssignment -> do
          putStrLn ("  " ++ name variableAssignment ++ "=" ++ value variableAssignment)
      Left error -> putStrLn ("Error: " ++ error)

  return ()
