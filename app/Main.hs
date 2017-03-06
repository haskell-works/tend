{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy as LT
import Data.Text.Lazy.IO as LTIO
import Data.Version (showVersion)
import Dhall
import HaskellWorks.Ci.Api.Circle
import HaskellWorks.Ci.Options
import HaskellWorks.Ci.Options.Cmd
import HaskellWorks.Ci.Types
import Network.Wreq
import Paths_hwa_ci (version)
import Prelude hiding (lines)
import System.Directory
import System.Process
import qualified Data.List.Extra as LE
import qualified HaskellWorks.Ci.Options as O
import qualified Options.Applicative as O
import qualified Prelude as P

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

-- | Conversion from a `Show` constrained type to `Text`
tshow :: Show a => a -> Text
tshow = pack . show

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

loadCircleConfig :: IO CircleConfig
loadCircleConfig = do
  home <- pack <$> getHomeDirectory
  input auto (home `append` "/.circle/config")

httpOptsFromCircleConfig :: CircleConfig -> Options
httpOptsFromCircleConfig circleConfig = let apiToken' = toStrict (apiToken circleConfig) in
  defaults & param "circle-token" .~ [apiToken'] & header "Accept" .~ ["application/json"]

main :: IO ()
main = do
  options <- O.execParser O.optionsParser
  case options ^. goptCmd of
    CmdOfCmdFromRemote cmd -> do
      remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
      let remoteEntries = catMaybes (LE.nub (remoteEntriesFromLine <$> remoteLines))
      LTIO.putStrLn $ "Remote entries: " <> tshow remoteEntries
    CmdOfCmdHelp cmd -> do
      LTIO.putStrLn "No help yet"
    CmdOfCmdPush cmd -> do
      circleConfig <- loadCircleConfig

      let opts = httpOptsFromCircleConfig circleConfig

      ciConfig :: CiConfig <- input auto "./ci.config"

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
    CmdOfCmdVersion cmd -> do
      P.putStrLn $ "hwa-ci " <> showVersion version
