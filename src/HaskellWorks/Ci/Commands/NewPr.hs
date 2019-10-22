{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HaskellWorks.Ci.Commands.NewPr
  ( cmdNewPr
  ) where

import Control.Monad
import HaskellWorks.Ci.Git
import Options.Applicative
import Web.Browser

import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.IO              as LTIO
import qualified HaskellWorks.Ci.Commands.Types as Z

runNewPr :: Z.NewPrOptions -> IO ()
runNewPr _ = do
  trackingBranchDetails <- gitTrackingBranchDetails
  case trackingBranchDetails of
    Right GitBranchDetails {..} -> do
      let url = "https://github.com/" <> gitBranchDetailsOrganisation <> "/" <> gitBranchDetailsProject <> "/compare/" <> gitBranchDetailsBranch <> "?expand=1"
      result <- openBrowser (LT.unpack url)
      unless result $ LTIO.putStrLn $ "Unable to open " <> url
    Left errorMsg -> LTIO.putStrLn errorMsg

optsNewPr :: Parser Z.NewPrOptions
optsNewPr = pure Z.NewPrOptions

cmdNewPr :: Mod CommandFields (IO ())
cmdNewPr = command "new-pr"  $ flip info idm $ runNewPr <$> optsNewPr
