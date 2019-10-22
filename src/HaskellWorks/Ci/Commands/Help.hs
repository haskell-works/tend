{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Commands.Help
  ( cmdHelp
  ) where

import Options.Applicative

import qualified Data.Text.Lazy.IO              as LTIO
import qualified HaskellWorks.Ci.Commands.Types as Z

runHelp :: Z.HelpOptions -> IO ()
runHelp _ = LTIO.putStrLn "No help yet"

optsHelp :: Parser Z.HelpOptions
optsHelp = pure Z.HelpOptions

cmdHelp :: Mod CommandFields (IO ())
cmdHelp = command "help"  $ flip info idm $ runHelp <$> optsHelp
