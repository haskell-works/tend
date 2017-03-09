{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module HaskellWorks.Ci.Action.Push where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Text.Lazy as LT
import Data.Text.Lazy.IO as LTIO
import Dhall
import HaskellWorks.Ci.Api.Circle
import HaskellWorks.Ci.Options
import HaskellWorks.Ci.Options.Cmd.Push
import HaskellWorks.Ci.Types
import Network.Wreq
import System.Directory

loadCircleConfig :: IO CircleConfig
loadCircleConfig = do
  home <- pack <$> getHomeDirectory
  input auto (home `append` "/.circle/config")

httpOptsFromCircleConfig :: CircleConfig -> Options
httpOptsFromCircleConfig circleConfig = let apiToken' = toStrict (apiToken circleConfig) in
  defaults & param "circle-token" .~ [apiToken'] & header "Accept" .~ ["application/json"]

actionPush :: CmdPush -> IO ()
actionPush cmd = do
  circleConfig <- loadCircleConfig

  let opts = httpOptsFromCircleConfig circleConfig

  ciConfig :: CiConfig <- input auto "./ci.config"

  projectsAssignments <- forConcurrently (projects ciConfig) $ \project -> do
    projectAssignments <- forConcurrently (projectVariables project) $ \projectVariableAssignment ->
      postCircleEnv opts "github" (projectOwner project) (projectName project) projectVariableAssignment
    return (project, projectAssignments)

  forM_ projectsAssignments $ \(project, projectAssignments) -> do
    LTIO.putStrLn $ "Uploading environment variables to: " <> projectOwner project <> "/" <> projectName project
    forM_ projectAssignments $ \variableAssignmentResult ->
      case variableAssignmentResult of
        Right variableAssignment  -> LTIO.putStrLn $ "  " <> name variableAssignment
        Left error                -> LTIO.putStrLn "Error"

  forM_ (projects ciConfig) $ \project -> do
    variableAssignmentsResult <- getCircleEnv opts "github" (projectOwner project) (projectName project)
    LTIO.putStrLn $ "Configured variables for: " <> projectOwner project <> "/" <> projectName project
    case variableAssignmentsResult of
      Right variableAssignments ->
        forM_ variableAssignments $ \variableAssignment -> do
          LTIO.putStrLn ("  " <> name variableAssignment <> "=" <> value variableAssignment)
          return ()
      Left error -> LTIO.putStrLn ("Error: " <> error)

  return ()
