{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Main where

import Circle.Types
import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Either.Combinators
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Lazy as LT
import Data.Text.IO as TIO
import Data.Text.Lazy.IO as LTIO
import Dhall
import Lib
import Network.Wreq
import Prelude hiding (lines)
import System.Directory
import System.IO
import System.Process
import Parser
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.Extra as LE
import qualified Data.Text as DT
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Prelude as P

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

-- | Conversion from a `Show` constrained type to `Text`
tshow :: Show a => a -> Text
tshow = pack . show

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

postCircleEnv :: W.Options -> Text -> Text -> Text -> VariableAssignment -> IO (Either Text VariableAssignment)
postCircleEnv opts vcsType username project assignment = do
  let url = "https://circleci.com/api/v1.1/project/" <> vcsType <> "/" <> username <> "/" <> project <> "/envvar"
  r <- postWith opts (unpack url) (toJSON assignment)
  case r ^. responseStatus . statusCode of
    code | code >= 200 && code < 300  -> return (mapLeft LT.pack (eitherDecode (r ^. responseBody) :: Either String VariableAssignment))
    _                                 -> return (Left "Error")

getCircleEnv :: W.Options -> Text -> Text -> Text -> IO (Either Text [VariableAssignment])
getCircleEnv opts vcsType username project = do
  let url = "https://circleci.com/api/v1.1/project/" <> vcsType <> "/" <> username <> "/" <> project <> "/envvar"
  r <- getWith opts (unpack url)
  case r ^. responseStatus . statusCode of
    code | code >= 200 && code < 300  -> return (mapLeft LT.pack (eitherDecode (r ^. responseBody) :: Either String [VariableAssignment]))
    _                                 -> return (Left "Error")

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
  -- LTIO.putStrLn $ "Remote entries: " <> tshow remoteEntries
  home <- pack <$> getHomeDirectory
  circleConfig <- input auto (home `append` "/.circle/config")
  -- LTIO.putStrLn $ "Circle config: " <> tshow (circleConfig :: CircleConfig)
  let apiToken' = toStrict (apiToken circleConfig)
  let opts = defaults & param "circle-token" .~ [apiToken'] & header "Accept" .~ ["application/json"]
  -- getMe opts

  ciConfig :: CiConfig <- input auto "./ci.config"
  -- print ciConfig

  projectsAssignments <- forConcurrently (projects ciConfig) $ \project -> do
    projectAssignments <- forConcurrently (projectVariables project) $ \projectVariableAssignment -> do
      postCircleEnv opts "github" (projectOwner project) (projectName project) projectVariableAssignment
    return (project, projectAssignments)

  forM_ projectsAssignments $ \(project, projectAssignments) -> do
    LTIO.putStrLn $ "Uploading environment variables to: " <> projectOwner project <> "/" <> projectName project
    forM_ projectAssignments $ \variableAssignmentResult -> do
      case variableAssignmentResult of
        Right variableAssignment  -> LTIO.putStrLn $ "  " <> name variableAssignment
        Left error                -> LTIO.putStrLn "Error"

  forM_ (projects ciConfig) $ \project -> do
    variableAssignmentsResult <- getCircleEnv opts "github" (projectOwner project) (projectName project)
    LTIO.putStrLn $ "Configured variables for: " <> projectOwner project <> "/" <> projectName project
    case variableAssignmentsResult of
      Right variableAssignments -> do
        forM_ variableAssignments $ \variableAssignment -> do
          LTIO.putStrLn ("  " <> name variableAssignment <> "=" <> value variableAssignment)
          return ()
      Left error -> LTIO.putStrLn ("Error: " <> error)

  return ()
