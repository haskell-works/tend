{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.Ci.Action.Push where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Text.Lazy                   as LT
import Data.Text.Lazy.IO                as LTIO
import HaskellWorks.Ci.Api.Circle
import HaskellWorks.Ci.Dhall
import HaskellWorks.Ci.Options.Cmd.Push
import HaskellWorks.Ci.Types
import Network.Wreq
import System.Directory

import qualified HaskellWorks.Ci.Types.VariableAssignment as VA

loadCircleConfig :: IO CircleConfig
loadCircleConfig = do
  home <- pack <$> getHomeDirectory
  input auto (LT.toStrict (home `append` "/.circle/config"))

httpOptsFromCircleConfig :: CircleConfig -> Options
httpOptsFromCircleConfig circleConfig = let apiToken' = (circleConfig ^. the @"apiToken") in
  defaults & param "circle-token" .~ [apiToken'] & header "Accept" .~ ["application/json"]

actionPush :: CmdPush -> IO ()
actionPush _ = do
  circleConfig <- loadCircleConfig

  let opts = httpOptsFromCircleConfig circleConfig

  ciConfig :: CiConfig <- input auto "./ci.config"

  projectsAssignments <- forConcurrently (ciConfig ^. the @"projects") $ \project -> do
    projectAssignments <- forConcurrently (project ^. the @"projectVariables") $ \projectVariableAssignment ->
      postCircleEnv opts "github" (fromStrict (project ^. the @"projectOwner")) (fromStrict (project ^. the @"projectName")) projectVariableAssignment
    return (project, projectAssignments)

  forM_ projectsAssignments $ \(project, projectAssignments) -> do
    LTIO.putStrLn $ "Uploading environment variables to: " <> fromStrict (project ^. the @"projectOwner") <> "/" <> fromStrict (project ^. the @"projectName")
    forM_ projectAssignments $ \variableAssignmentResult ->
      case variableAssignmentResult of
        Right variableAssignment -> LTIO.putStrLn $ "  " <> fromStrict (variableAssignment ^. the @"name")
        Left e                   -> LTIO.putStrLn $ "Error: " <> e

  forM_ (projects ciConfig) $ \project -> do
    variableAssignmentsResult <- getCircleEnv opts "github" (fromStrict (project ^. the @"projectOwner")) (fromStrict (project ^. the @"projectName"))
    LTIO.putStrLn $ "Configured variables for: " <> fromStrict (project ^. the @"projectOwner") <> "/" <> fromStrict (project ^. the @"projectName")
    case variableAssignmentsResult of
      Right variableAssignments ->
        forM_ variableAssignments $ \variableAssignment -> do
          LTIO.putStrLn ("  " <> fromStrict (variableAssignment ^. the @"name") <> "=" <> fromStrict (variableAssignment ^. the @"value"))
          return ()
      Left e -> LTIO.putStrLn ("Error: " <> e)

  return ()
