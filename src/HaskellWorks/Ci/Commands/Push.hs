{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.Ci.Commands.Push
  ( cmdPush
  ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import HaskellWorks.Ci.Api.Circle
import HaskellWorks.Ci.Dhall
import HaskellWorks.Ci.Types
import Network.Wreq
import Options.Applicative
import System.Directory

import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.IO                        as LTIO
import qualified HaskellWorks.Ci.Commands.Types           as Z
import qualified HaskellWorks.Ci.Dhall                    as D
import qualified HaskellWorks.Ci.Types.VariableAssignment as VA
import qualified Network.Wreq                             as W

loadCircleConfig :: IO CircleConfig
loadCircleConfig = do
  home <- LT.pack <$> getHomeDirectory
  input D.auto (LT.toStrict (home <> "/.circle/config"))

httpOptsFromCircleConfig :: CircleConfig -> Options
httpOptsFromCircleConfig circleConfig = let apiToken' = (circleConfig ^. the @"apiToken") in
  defaults & param "circle-token" .~ [apiToken'] & W.header "Accept" .~ ["application/json"]

runPush :: Z.PushOptions -> IO ()
runPush _ = do
  circleConfig <- loadCircleConfig

  let opts = httpOptsFromCircleConfig circleConfig

  ciConfig :: CiConfig <- input D.auto "./ci.config"

  projectsAssignments <- forConcurrently (projects ciConfig) $ \project -> do
    projectAssignments <- forConcurrently (projectVariables project) $ \projectVariableAssignment ->
      postCircleEnv opts "github" (LT.fromStrict (projectOwner project)) (LT.fromStrict (projectName project)) projectVariableAssignment
    return (project, projectAssignments)

  forM_ projectsAssignments $ \(project, projectAssignments) -> do
    LTIO.putStrLn $ "Uploading environment variables to: " <> LT.fromStrict (projectOwner project) <> "/" <> LT.fromStrict (projectName project)
    forM_ projectAssignments $ \variableAssignmentResult ->
      case variableAssignmentResult of
        Right variableAssignment -> LTIO.putStrLn $ "  " <> LT.fromStrict (name variableAssignment)
        Left e                   -> LTIO.putStrLn $ "Error: " <> e

  forM_ (projects ciConfig) $ \project -> do
    variableAssignmentsResult <- getCircleEnv opts "github" (LT.fromStrict (projectOwner project)) (LT.fromStrict (projectName project))
    LTIO.putStrLn $ "Configured variables for: " <> LT.fromStrict (projectOwner project) <> "/" <> LT.fromStrict (projectName project)
    case variableAssignmentsResult of
      Right variableAssignments ->
        forM_ variableAssignments $ \variableAssignment -> do
          LTIO.putStrLn ("  " <> LT.fromStrict (name variableAssignment) <> "=" <> LT.fromStrict (VA.value variableAssignment))
          return ()
      Left e -> LTIO.putStrLn ("Error: " <> e)

  return ()

optsPush :: Parser Z.PushOptions
optsPush = pure Z.PushOptions

cmdPush :: Mod CommandFields (IO ())
cmdPush = command "push"  $ flip info idm $ runPush <$> optsPush
