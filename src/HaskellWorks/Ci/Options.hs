{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Ci.Options where

import Data.Version
import GHC.Generics
import Options.Applicative

import HaskellWorks.Ci.Options.Cmd

data HelpOptions = HelpOptions deriving (Show, Eq)

newtype GlobalOptions = GlobalOptions
  { cmd :: Cmd
  }
  deriving (Show, Eq, Generic)

parserGlobalOptions :: Parser GlobalOptions
parserGlobalOptions = GlobalOptions <$> cmds

optionsParser :: Version -> String -> ParserInfo GlobalOptions
optionsParser version gitHash = info (helper <*> versionOption <*> parserGlobalOptions)
  (  fullDesc
  <> progDesc "CLI tool for Continuous Integration"
  <> header "CI tool"
  )
  where versionOption = infoOption
          (concat [showVersion version, " ", gitHash])
          (long "version" <> help "Show version")
