module HaskellWorks.Ci.Text where

import Data.Text.Lazy

-- | Conversion from a `Show` constrained type to `Text`
tshow :: Show a => a -> Text
tshow = pack . show
