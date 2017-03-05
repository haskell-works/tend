module HaskellWorks.Ci.Options.Internal
  ( readOption
  ) where

import Options.Applicative
import Text.Read

readOption :: Read a => Mod OptionFields a -> Parser a
readOption = option $ eitherReader readEither
