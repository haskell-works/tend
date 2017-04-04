{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module HaskellWorks.Ci.Action.OpenCi where

import Control.Monad
import Data.Monoid
import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Git
import HaskellWorks.Ci.Options.Cmd.OpenCi
import Web.Browser
import qualified Data.Text.Lazy as LT

actionOpenCi :: CmdOpenCi -> IO ()
actionOpenCi _ = do
  trackingBranchDetails <- gitTrackingBranchDetails
  case trackingBranchDetails of
    Right GitBranchDetails {..} -> do
      let url = "https://circleci.com/gh/" <> gitBranchDetailsOrganisation <> "/" <> gitBranchDetailsProject <> "/tree/" <> gitBranchDetailsBranch
      result <- openBrowser (LT.unpack url)
      unless result $ LTIO.putStrLn $ "Unable to open " <> url
    Left errorMsg -> LTIO.putStrLn errorMsg
