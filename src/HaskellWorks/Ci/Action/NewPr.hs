{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module HaskellWorks.Ci.Action.NewPr where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as LTIO
import HaskellWorks.Ci.Git
import HaskellWorks.Ci.Options.Cmd.NewPr
import HaskellWorks.Ci.Text
import Web.Browser
import qualified Data.Text.Lazy as LT

actionNewPr :: CmdNewPr -> IO ()
actionNewPr cmd = do
  trackingBranchDetails <- gitTrackingBranchDetails
  case trackingBranchDetails of
    Right GitBranchDetails {..} -> do
      let url = "https://github.com/" <> gitBranchDetailsOrganisation <> "/" <> gitBranchDetailsProject <> "/compare/" <> gitBranchDetailsBranch <> "?expand=1"
      result <- openBrowser (LT.unpack url)
      unless result $ LTIO.putStrLn $ "Unable to open " <> url
    Left errorMsg -> LTIO.putStrLn errorMsg
