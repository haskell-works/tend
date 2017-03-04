module Parser.Types where

import Data.Text

newtype User = User { user :: Text } deriving (Eq, Show)

newtype Host = Host { host :: Text } deriving (Eq, Show)

newtype Segment = Segment { segment :: Text } deriving (Eq, Show)

data Authority = Authority
  { authorityUser :: Maybe User
  , authorityHost :: Host
  } deriving (Eq, Show)
