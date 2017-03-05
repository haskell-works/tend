module HaskellWorks.Ci.Options where

import Control.Lens
import Data.Monoid
import Options.Applicative

import HaskellWorks.Ci.Options.Cmd
import HaskellWorks.Ci.Options.Internal

data HelpOptions = HelpOptions deriving (Show, Eq)

newtype GlobalOptions = GlobalOptions
  { _goptCmd :: Cmd
  }
  deriving (Show, Eq)

parserGlobalOptions :: Parser GlobalOptions
parserGlobalOptions = GlobalOptions <$> cmds

optionsParser :: ParserInfo GlobalOptions
optionsParser = info (helper <*> parserGlobalOptions)
  (  fullDesc
  <> progDesc "CLI tool for Continuous Integration"
  <> header "CI tool"
  )
