{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HaskellWorks.Ci.Commands.OpenCi
  ( cmdOpenCi
  ) where

import Control.Monad
import HaskellWorks.Ci.Git
import Options.Applicative
import Web.Browser

import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.IO              as LTIO
import qualified HaskellWorks.Ci.Commands.Types as Z

runOpenCi :: Z.OpenCiOptions -> IO ()
runOpenCi _ = do
  trackingBranchDetails <- gitTrackingBranchDetails
  case trackingBranchDetails of
    Right GitBranchDetails {..} -> do
      let url = "https://circleci.com/gh/" <> gitBranchDetailsOrganisation <> "/" <> gitBranchDetailsProject <> "/tree/" <> gitBranchDetailsBranch
      result <- openBrowser (LT.unpack url)
      unless result $ LTIO.putStrLn $ "Unable to open " <> url
    Left errorMsg -> LTIO.putStrLn errorMsg

optsOpenCi :: Parser Z.OpenCiOptions
optsOpenCi = pure Z.OpenCiOptions

cmdOpenCi :: Mod CommandFields (IO ())
cmdOpenCi = command "open-ci"  $ flip info idm $ runOpenCi <$> optsOpenCi

